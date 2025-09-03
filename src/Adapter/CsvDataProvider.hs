{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Adapter.CsvDataProvider where

import Domain.Services.BacktestService (DataProvider(..), DateRange(..), CandlePeriod(..), DataQualityReport(..))
import Domain.Types
import Util.Error (Result, AppError(..), fileError, dataError)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import System.Directory (listDirectory, doesDirectoryExist, createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import Data.Maybe (mapMaybe)
import Data.Scientific (fromFloatDigits, Scientific)
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale, addUTCTime)
import Data.List (sortOn)
import Control.Exception (catch, IOException)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS

-- CSV adapter implementation of DataProvider port
newtype CsvDataProvider = CsvDataProvider
  { csvDataDirectory :: FilePath
  }

-- Monad for CSV data provider
newtype CsvDataProviderM m a = CsvDataProviderM
  { runCsvDataProvider :: ReaderT CsvDataProvider m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader CsvDataProvider)

instance MonadIO m => DataProvider (CsvDataProviderM m) where
  loadTicks instrument dateRange = do
    dataDir <- asks csvDataDirectory
    liftIO $ loadTicksFromCsv dataDir instrument dateRange

  loadCandles instrument dateRange period = do
    dataDir <- asks csvDataDirectory
    let cachePath = candleCachePath dataDir instrument dateRange period
    cacheExists <- liftIO $ doesFileExist cachePath
    if cacheExists
      then do
        cached <- liftIO $ tryIO $ LBS.readFile cachePath
        case cached of
          Left _ -> computeAndCache dataDir cachePath instrument dateRange period
          Right lbs -> case JSON.decode lbs :: Maybe [Candle] of
            Just cs -> pure $ Right cs
            Nothing -> computeAndCache dataDir cachePath instrument dateRange period
      else computeAndCache dataDir cachePath instrument dateRange period

  validateDataQuality ticks =
    pure $ Right $ analyzeDataQuality ticks

-- compute from ticks and write cache
computeAndCache :: (Monad m, MonadIO m) => FilePath -> FilePath -> Instrument -> DateRange -> CandlePeriod -> CsvDataProviderM m (Result [Candle])
computeAndCache dataDir cachePath instrument dateRange period = do
  ticksResult <- loadTicks instrument dateRange
  case ticksResult of
    Left err -> return $ Left err
    Right ticks -> do
      let candles = aggregateTicksToCandles period ticks
      _ <- liftIO $ tryIO $ createDirectoryIfMissing True (takeDirectory cachePath)
      _ <- liftIO $ tryIO $ LBS.writeFile cachePath (JSON.encode candles)
      return $ Right candles

-- Implementation functions
loadTicksFromCsv :: FilePath -> Instrument -> DateRange -> IO (Result [Tick])
loadTicksFromCsv dataDir instrument dateRange = do
  dirExists <- doesDirectoryExist dataDir
  if not dirExists
    then pure $ Left $ fileError $ "Data directory does not exist: " <> T.pack dataDir
    else do
      filesResult <- getMatchingFiles dataDir instrument dateRange
      case filesResult of
        Left err -> pure $ Left err
        Right filePaths -> do
          allTickResults <- mapM (loadTicksFromFile dataDir) filePaths
          let (errors, tickLists) = partitionEithers allTickResults
          if null errors
            then pure $ Right $ sortOn tTime $ concat tickLists
            else pure $ Left $ fileError $ "File loading errors: " <> T.intercalate "; " (map (T.pack . show) errors)

getMatchingFiles :: FilePath -> Instrument -> DateRange -> IO (Result [String])
getMatchingFiles dataDir instrument dateRange = do
  filesResult <- tryIO $ listDirectory dataDir
  case filesResult of
    Left err -> pure $ Left $ fileError $ T.pack $ show err
    Right allFiles ->
      let csvFiles = filter (T.isPrefixOf "DAT_ASCII_" . T.pack) allFiles
          instrumentFiles = filter (isFileForInstrument instrument) csvFiles
          dateFilteredFiles = filter (isFileInDateRange dateRange) instrumentFiles
      in pure $ Right dateFilteredFiles

loadTicksFromFile :: FilePath -> String -> IO (Result [Tick])
loadTicksFromFile dataDir fileName = do
  contentResult <- tryIO $ TIO.readFile (dataDir </> fileName)
  case contentResult of
    Left err -> pure $ Left $ fileError $ T.pack $ show err
    Right content ->
      case extractInstrumentFromFilename fileName of
        Nothing -> pure $ Left $ dataError $ "Could not parse instrument from: " <> T.pack fileName
        Just instrument -> do
          let ticks = parseTicksFromText instrument content
          pure $ Right ticks

aggregateTicksToCandles :: CandlePeriod -> [Tick] -> [Candle]
aggregateTicksToCandles period ticks =
  let periodSeconds = case period of
        OneMinute -> 60
        FiveMinute -> 300
        FifteenMinute -> 900
        OneHour -> 3600
        FourHour -> 14400
        Daily -> 86400
  in groupTicksIntoCandles periodSeconds ticks

groupTicksIntoCandles :: Int -> [Tick] -> [Candle]
groupTicksIntoCandles periodSeconds ticks =
  let sortedTicks = sortOn tTime ticks
      tickGroups = groupByTimePeriod periodSeconds sortedTicks
  in mapMaybe convertTickGroupToCandle tickGroups

analyzeDataQuality :: [Tick] -> DataQualityReport
analyzeDataQuality ticks =
  let tickCount = length ticks
      gaps = countDataGaps ticks
      outliers = countOutliers ticks
      avgSpread = calculateAverageSpread ticks
      qualityScore = max 0 $ min 100 $ fromIntegral (100 - gaps - outliers * 2)
  in DataQualityReport tickCount gaps outliers avgSpread qualityScore

-- Helper functions
tryIO :: IO a -> IO (Either IOException a)
tryIO action = (Right <$> action) `catch` (pure . Left)

isFileForInstrument :: Instrument -> String -> Bool
isFileForInstrument (Instrument targetInstr) fileName =
  case T.splitOn "_" (T.pack fileName) of
    [_, _, instrStr, _, _] -> instrStr == targetInstr
    _ -> False

isFileInDateRange :: DateRange -> String -> Bool
isFileInDateRange dateRange fileName =
  case extractDateFromFilename fileName of
    Nothing -> False
    Just (year, month) ->
      let fileDate = year * 100 + month
          startDate = drStartYear dateRange * 100 + drStartMonth dateRange
          endDate = drEndYear dateRange * 100 + drEndMonth dateRange
      in fileDate >= startDate && fileDate <= endDate

extractInstrumentFromFilename :: String -> Maybe Instrument
extractInstrumentFromFilename fileName =
  case T.splitOn "_" (T.pack fileName) of
    [_, _, instrStr, _, _] -> Just $ Instrument instrStr
    _ -> Nothing

extractDateFromFilename :: String -> Maybe (Int, Int)
extractDateFromFilename fileName =
  case T.splitOn "_" (T.pack fileName) of
    [_, _, _, _, dateStr] ->
      let dateOnly = T.takeWhile (/= '.') dateStr
      in if T.length dateOnly == 6
         then case (T.take 4 dateOnly, T.drop 4 dateOnly) of
           (yearStr, monthStr) ->
             case (TR.decimal yearStr, TR.decimal monthStr) of
               (Right (year, _), Right (month, _)) -> Just (year, month)
               _ -> Nothing
         else Nothing
    _ -> Nothing

parseTicksFromText :: Instrument -> T.Text -> [Tick]
parseTicksFromText instrument content =
  mapMaybe (parseTickLine instrument) (T.lines content)

parseTickLine :: Instrument -> T.Text -> Maybe Tick
parseTickLine instr line =
  case T.splitOn "," line of
    (dt:bid:ask:_) -> do
      time <- parseDateTime (T.unpack dt)
      bidPrice <- parsePrice bid
      askPrice <- parsePrice ask
      return $ Tick time instr bidPrice askPrice Nothing
    _ -> Nothing
  where
    parsePrice :: T.Text -> Maybe Price
    parsePrice priceText =
      case TR.double priceText of
        Right (price, _) -> Just $ Price $ fromFloatDigits price
        Left _ -> Nothing

parseDateTime :: String -> Maybe UTCTime
parseDateTime dateTimeStr =
  case words dateTimeStr of
    [dateStr, timeStr] -> do
      let year = take 4 dateStr
          month = take 2 $ drop 4 dateStr
          day = take 2 $ drop 6 dateStr
          dateFormatted = year ++ "-" ++ month ++ "-" ++ day

          hour = take 2 timeStr
          minute = take 2 $ drop 2 timeStr
          second = take 2 $ drop 4 timeStr
          millis = take 3 $ drop 6 timeStr ++ "000"
          timeFormatted = hour ++ ":" ++ minute ++ ":" ++ second ++ "." ++ millis

      parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" (dateFormatted ++ " " ++ timeFormatted)
    _ -> Nothing

-- Simplified implementations for missing functions
groupByTimePeriod :: Int -> [Tick] -> [[Tick]]
groupByTimePeriod _ [] = []
groupByTimePeriod periodSeconds ticks =
  let sortedTicks = sortOn tTime ticks
  in groupTicksByTimePeriod periodSeconds sortedTicks

-- Properly group ticks by time periods
groupTicksByTimePeriod :: Int -> [Tick] -> [[Tick]]
groupTicksByTimePeriod _ [] = []
groupTicksByTimePeriod periodSeconds (firstTick:restTicks) =
  let startTime = tTime firstTick
      periodEnd = addUTCTime (fromIntegral periodSeconds) startTime
      (currentGroup, remaining) = span (\tick -> tTime tick < periodEnd) (firstTick:restTicks)
  in currentGroup : groupTicksByTimePeriod periodSeconds remaining

convertTickGroupToCandle :: [Tick] -> Maybe Candle
convertTickGroupToCandle [] = Nothing
convertTickGroupToCandle ticks@(firstTick:_) =
  let prices = map (\t -> Price $ (unPrice (tBid t) + unPrice (tAsk t)) / 2) ticks
      openPrice = head prices
      closePrice = last prices
      highPrice = Price $ maximum $ map unPrice prices
      lowPrice = Price $ minimum $ map unPrice prices
  in Just $ Candle (tTime firstTick) openPrice highPrice lowPrice closePrice

countDataGaps :: [Tick] -> Int
countDataGaps _ = 0 -- Simplified implementation

countOutliers :: [Tick] -> Int
countOutliers _ = 0 -- Simplified implementation

calculateAverageSpread :: [Tick] -> Scientific
calculateAverageSpread [] = 0
calculateAverageSpread ticks =
  let spreads = map (\t -> unPrice (tAsk t) - unPrice (tBid t)) ticks
  in sum spreads / fromIntegral (length spreads)

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr (either left right) ([], [])
  where
    left  a ~(l, r) = (a:l, r)
    right b ~(l, r) = (l, b:r)

-- Helper functions for candle caching
candleCachePath :: FilePath -> Instrument -> DateRange -> CandlePeriod -> FilePath
candleCachePath dataDir (Instrument instr) dr period =
  let cacheDir = dataDir </> ".cache" </> "candles"
      tag = T.unpack instr <> "_" <> show (drStartYear dr) <> pad (drStartMonth dr)
            <> "_" <> show (drEndYear dr) <> pad (drEndMonth dr)
            <> "_" <> periodTag period
  in cacheDir </> (tag <> ".json")
  where
    pad m = if m < 10 then '0':show m else show m

periodTag :: CandlePeriod -> String
periodTag OneMinute = "M1"
periodTag FiveMinute = "M5"
periodTag FifteenMinute = "M15"
periodTag OneHour = "H1"
periodTag FourHour = "H4"
periodTag Daily = "D1"
