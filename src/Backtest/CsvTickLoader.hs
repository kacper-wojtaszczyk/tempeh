{-# LANGUAGE OverloadedStrings #-}
module Backtest.CsvTickLoader
  ( loadTicksWithConfig
  , DateRange(..)
  , DataFilter(..)
  , TickLoadResult
  ) where

import Domain.Types
import Util.Config (AppConfig(..), LogConfig(..))
import Util.Error (AppError(..), Result)
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>))
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (catch, IOException)
import Data.Maybe (mapMaybe)
import Data.Scientific (fromFloatDigits)
import System.IO (hFlush, stdout)

-- Type aliases for clarity
type TickLoadResult = Result [Tick]
type FileFilter = String -> Bool

-- Date range for filtering files
data DateRange = DateRange
  { startYear :: Int
  , startMonth :: Int
  , endYear :: Int
  , endMonth :: Int
  } deriving (Show, Eq)

-- Data filtering options
data DataFilter
  = AllData
  | DateRangeFilter DateRange
  | InstrumentFilter Instrument
  | CombinedFilter DataFilter DataFilter
  deriving (Show, Eq)

-- Main loading function with extensive debugging
loadTicksWithConfig :: MonadIO m => AppConfig -> logger -> Instrument -> DataFilter -> m TickLoadResult
loadTicksWithConfig config _ instrument dataFilter = liftIO $ do
  putStrLn $ "=== CSV TICK LOADER DEBUG ==="
  putStrLn $ "Data directory: " <> acDataDirectory config
  putStrLn $ "Target instrument: " <> show instrument
  putStrLn $ "Data filter: " <> show dataFilter
  hFlush stdout

  -- Validate directory exists
  dirExists <- doesDirectoryExist (acDataDirectory config)
  putStrLn $ "Directory exists: " <> show dirExists
  hFlush stdout

  if not dirExists
    then do
      putStrLn "ERROR: Data directory does not exist!"
      pure $ Left $ FileError $ "Data directory does not exist: " <> T.pack (acDataDirectory config)
    else do
      -- Get filtered files with debugging
      filesResult <- getFilteredFiles (acDataDirectory config) instrument dataFilter
      case filesResult of
        Left err -> do
          putStrLn $ "ERROR getting files: " <> show err
          pure $ Left err
        Right files -> do
          putStrLn $ "Found " <> show (length files) <> " files to process: " <> show files
          hFlush stdout
          loadTicksFromFiles (acDataDirectory config) files

-- Get filtered files with extensive debugging
getFilteredFiles :: FilePath -> Instrument -> DataFilter -> IO (Result [String])
getFilteredFiles dataDir instrument dataFilter = do
  putStrLn $ "Listing files in directory: " <> dataDir
  hFlush stdout

  filesResult <- tryIO $ listDirectory dataDir
  case filesResult of
    Left err -> do
      putStrLn $ "Failed to list directory: " <> show err
      pure $ Left $ FileError $ T.pack $ show err
    Right allFiles -> do
      putStrLn $ "All files in directory: " <> show allFiles
      let csvFiles = filter (T.isPrefixOf "DAT_ASCII_" . T.pack) allFiles
      putStrLn $ "CSV files found: " <> show csvFiles

      let fileFilter = createFileFilter instrument dataFilter
          filteredFiles = filter fileFilter csvFiles
      putStrLn $ "Files after filtering: " <> show filteredFiles
      hFlush stdout

      pure $ Right filteredFiles
  where
    tryIO :: IO a -> IO (Either IOError a)
    tryIO action = (Right <$> action) `catch` (pure . Left)

-- Pure function to get file filter based on DataFilter
createFileFilter :: Instrument -> DataFilter -> FileFilter
createFileFilter instrument = go
  where
    go AllData = isFileForInstrument instrument
    go (DateRangeFilter range) = \f ->
      let instrMatch = isFileForInstrument instrument f
          rangeMatch = isFileInRange range f
      in instrMatch && rangeMatch
    go (InstrumentFilter instr) = isFileForInstrument instr
    go (CombinedFilter f1 f2) = \f -> go f1 f && go f2 f

-- Load ticks from files with debugging
loadTicksFromFiles :: FilePath -> [String] -> IO TickLoadResult
loadTicksFromFiles dataDir files = do
  putStrLn $ "Loading ticks from " <> show (length files) <> " files"
  hFlush stdout

  results <- forM files $ \file -> do
    putStrLn $ "Processing file: " <> file
    hFlush stdout
    loadTicksFromFile (dataDir </> file)

  -- Combine results, collecting errors
  let (errors, tickLists) = partitionEithers results
  if null errors
    then do
      let allTicks = concat tickLists
      putStrLn $ "Successfully loaded " <> show (length allTicks) <> " total ticks"
      hFlush stdout
      pure $ Right allTicks
    else do
      putStrLn $ "Errors encountered: " <> show errors
      pure $ Left $ FileError $ "Multiple file errors: " <> T.intercalate "; " (map errorMessage errors)

-- Load ticks from a single file with debugging
loadTicksFromFile :: FilePath -> IO TickLoadResult
loadTicksFromFile filePath = do
  putStrLn $ "Reading file: " <> filePath
  hFlush stdout

  contentResult <- tryReadFile filePath
  case contentResult of
    Left err -> do
      putStrLn $ "Failed to read file: " <> show err
      pure $ Left $ FileError $ T.pack $ show err
    Right contents -> do
      putStrLn $ "File read successfully, content length: " <> show (T.length contents)

      case extractInstrumentFromFilename (takeFileName filePath) of
        Nothing -> do
          putStrLn $ "Could not extract instrument from filename: " <> filePath
          pure $ Left $ ParseError $ "Could not extract instrument from filename: " <> T.pack filePath
        Just instrument -> do
          putStrLn $ "Extracted instrument: " <> show instrument
          let ticks = parseTicksFromText instrument contents
          putStrLn $ "Parsed " <> show (length ticks) <> " ticks from " <> filePath
          hFlush stdout
          pure $ Right ticks
  where
    tryReadFile :: FilePath -> IO (Either IOError Text)
    tryReadFile path = (Right <$> TIO.readFile path) `catch` (pure . Left)

    takeFileName = reverse . takeWhile (/= '/') . reverse

-- Pure parsing function
parseTicksFromText :: Instrument -> Text -> [Tick]
parseTicksFromText instrument content =
  let lines = T.lines content
      nonEmptyLines = filter (not . T.null) lines
      ticks = mapMaybe (parseTickLine instrument) nonEmptyLines
  in ticks

-- Improved parsing with better error handling
parseTickLine :: Instrument -> Text -> Maybe Tick
parseTickLine instr line =
  case T.splitOn "," line of
    (dt:bid:ask:_) -> do
      time <- parseDateTime (T.unpack dt)
      bid' <- parsePrice bid
      ask' <- parsePrice ask
      return $ Tick time instr bid' ask'
    _ -> Nothing
  where
    parsePrice :: Text -> Maybe Price
    parsePrice priceText =
      case TR.double priceText of
        Right (price, _) -> Just $ Price $ fromFloatDigits price
        Left _ -> Nothing

-- Parse the custom datetime format: "20250101 170014647"
parseDateTime :: String -> Maybe UTCTime
parseDateTime dateTimeStr =
  case words dateTimeStr of
    [dateStr, timeStr] -> do
      -- Parse date part: "20250101" -> "2025-01-01"
      let year = take 4 dateStr
          month = take 2 $ drop 4 dateStr
          day = take 2 $ drop 6 dateStr
          dateFormatted = year ++ "-" ++ month ++ "-" ++ day

      -- Parse time part: "170014647" -> "17:00:14.647"
      let hour = take 2 timeStr
          minute = take 2 $ drop 2 timeStr
          second = take 2 $ drop 4 timeStr
          millis = take 3 $ drop 6 timeStr ++ "000" -- pad with zeros if needed
          timeFormatted = hour ++ ":" ++ minute ++ ":" ++ second ++ "." ++ millis

      parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" (dateFormatted ++ " " ++ timeFormatted)
    _ -> Nothing

extractDateFromFilename :: String -> Maybe (Int, Int)
extractDateFromFilename fileName =
  case T.splitOn "_" (T.pack fileName) of
    [_, _, _, _, dateStr] ->  -- Changed from 4 to 5 parts: DAT_ASCII_EURUSD_T_202503
      let dateOnly = T.takeWhile (/= '.') dateStr
      in if T.length dateOnly == 6
         then case (T.take 4 dateOnly, T.drop 4 dateOnly) of
           (yearStr, monthStr) ->
             case (TR.decimal yearStr, TR.decimal monthStr) of
               (Right (year, _), Right (month, _)) -> Just (year, month)
               _ -> Nothing
         else Nothing
    _ -> Nothing

isFileInRange :: DateRange -> String -> Bool
isFileInRange dateRange fileName =
  case extractDateFromFilename fileName of
    Just (year, month) ->
      let fileDate = year * 100 + month
          startDate = startYear dateRange * 100 + startMonth dateRange
          endDate = endYear dateRange * 100 + endMonth dateRange
      in fileDate >= startDate && fileDate <= endDate
    Nothing -> False

extractInstrumentFromFilename :: String -> Maybe Instrument
extractInstrumentFromFilename fileName =
  case T.splitOn "_" (T.pack fileName) of
    [_, _, instrStr, _, _] ->
      -- Accept any valid instrument string instead of hardcoded values
      if T.null instrStr
        then Nothing
        else Just (Instrument instrStr)
    _ -> Nothing

isFileForInstrument :: Instrument -> String -> Bool
isFileForInstrument instrument fileName =
  case extractInstrumentFromFilename fileName of
    Just fileInstr -> fileInstr == instrument
    Nothing -> False

-- Helper function missing from imports
partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr (either left right) ([], [])
  where
    left  a ~(l, r) = (a:l, r)
    right b ~(l, r) = (l, b:r)

errorMessage :: AppError -> Text
errorMessage (ParseError msg) = "Parse error: " <> msg
errorMessage (FileError msg) = "File error: " <> msg
errorMessage (ConfigError msg) = "Configuration error: " <> msg
errorMessage (BacktestError msg) = "Backtest error: " <> msg
errorMessage (ValidationError msg) = "Validation error: " <> msg
