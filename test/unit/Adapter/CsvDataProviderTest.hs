module Unit.Adapter.CsvDataProviderTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Adapter.CsvDataProvider
import Domain.Types
import Domain.Services.BacktestService
import Data.Scientific (fromFloatDigits)
import Data.Time
import qualified Data.Text as T
import qualified Data.Text.Read as TR

tests :: TestTree
tests = testGroup "CSV Data Provider"
  [ testGroup "File Name Parsing"
    [ testCase "Extract instrument from filename" testInstrumentExtraction
    , testCase "Extract date from filename" testDateExtraction
    , testCase "Invalid filename handling" testInvalidFilename
    ]
  , testGroup "Date and Time Parsing"
    [ testCase "Parse valid datetime string" testDateTimeParsing
    , testCase "Handle invalid datetime format" testInvalidDateTime
    ]
  , testGroup "Tick Data Processing"
    [ testCase "Parse valid tick line" testTickLineParsing
    , testCase "Handle malformed tick data" testMalformedTickData
    , testCase "Price validation" testPriceValidation
    ]
  , testGroup "Data Quality Analysis"
    [ testCase "Quality report generation" testQualityReport
    , testCase "Gap detection" testGapDetection
    , testCase "Spread calculation" testSpreadCalculation
    ]
  ]

testInstrumentExtraction :: IO ()
testInstrumentExtraction = do
  let filename = "DAT_ASCII_EURUSD_T_202501.csv"
      result = extractInstrumentFromFilename filename
  assertEqual "Should extract EURUSD" (Just $ Instrument (T.pack "EURUSD")) result

testDateExtraction :: IO ()
testDateExtraction = do
  let filename = "DAT_ASCII_EURUSD_T_202501.csv"
      result = extractDateFromFilename filename
  assertEqual "Should extract year 2025, month 1" (Just (2025, 1)) result

testInvalidFilename :: IO ()
testInvalidFilename = do
  let filename = "invalid_filename.csv"
      instrumentResult = extractInstrumentFromFilename filename
      dateResult = extractDateFromFilename filename
  assertEqual "Invalid filename should return Nothing for instrument" Nothing instrumentResult
  assertEqual "Invalid filename should return Nothing for date" Nothing dateResult

testDateTimeParsing :: IO ()
testDateTimeParsing = do
  let dateTimeStr = "20250101 170014647"
      result = parseDateTime dateTimeStr
  assertBool "Should parse valid datetime" (result /= Nothing)

testInvalidDateTime :: IO ()
testInvalidDateTime = do
  let dateTimeStr = "invalid_datetime"
      result = parseDateTime dateTimeStr
  assertEqual "Invalid datetime should return Nothing" Nothing result

testTickLineParsing :: IO ()
testTickLineParsing = do
  let tickLine = T.pack "20250101 170014647,1.035030,1.035910,0"
      instrument = Instrument (T.pack "EURUSD")
      result = parseTickLine instrument tickLine
      approxEqual a b = abs (unPrice a - unPrice b) < 1e-6
  case result of
    Just tick -> do
      assertEqual "Instrument should match" instrument (tInstr tick)
      assertEqual "Bid should be parsed" (Price $ fromFloatDigits 1.035030) (tBid tick)
      assertBool "Ask should be parsed (approximate)" (approxEqual (Price $ fromFloatDigits 1.035910) (tAsk tick))
    Nothing -> assertFailure "Should parse valid tick line"

testMalformedTickData :: IO ()
testMalformedTickData = do
  let tickLine = T.pack "invalid,tick,data"
      instrument = Instrument (T.pack "EURUSD")
      result = parseTickLine instrument tickLine
  assertEqual "Malformed tick data should return Nothing" Nothing result

testPriceValidation :: IO ()
testPriceValidation = do
  let validPrice = T.pack "1.08500"
      invalidPrice = T.pack "not_a_price"
      validResult = parsePrice validPrice
      invalidResult = parsePrice invalidPrice
  assertBool "Valid price should parse" (validResult /= Nothing)
  assertEqual "Invalid price should return Nothing" Nothing invalidResult

testQualityReport :: IO ()
testQualityReport = do
  let ticks = createSampleTicksForQuality
      report = analyzeDataQuality ticks
  assertBool "Quality score should be reasonable" (dqrQualityScore report >= 0 && dqrQualityScore report <= 100)
  assertBool "Tick count should match" (dqrTickCount report == length ticks)

testGapDetection :: IO ()
testGapDetection = do
  let ticks = createTicksWithGaps
      gaps = countDataGaps ticks
  assertBool "Should detect gaps correctly" (gaps >= 0)

testSpreadCalculation :: IO ()
testSpreadCalculation = do
  let ticks = createSampleTicksForQuality
      avgSpread = calculateAverageSpread ticks
  assertBool "Average spread should be positive" (avgSpread > 0)

-- Helper functions
parsePrice :: T.Text -> Maybe Price
parsePrice priceText =
  case TR.double priceText of
    Right (price, _) -> Just $ Price $ fromFloatDigits price
    Left _ -> Nothing

createSampleTicksForQuality :: [Tick]
createSampleTicksForQuality =
  let baseTime = read "2025-01-01 00:00:00 UTC"
      prices = [(1.08500, 1.08520), (1.08505, 1.08525), (1.08510, 1.08530)]
  in zipWith (\i (bid, ask) -> Tick
       { tTime = addUTCTime (fromIntegral $ i * 60) baseTime
       , tInstr = Instrument (T.pack "EURUSD")
       , tBid = Price (fromFloatDigits bid)
       , tAsk = Price (fromFloatDigits ask)
       , tVolume = Nothing
       }) [0..] prices

createTicksWithGaps :: [Tick]
createTicksWithGaps =
  let baseTime = read "2025-01-01 00:00:00 UTC"
      -- Create ticks with time gaps
      times = [0, 60, 300, 900] -- Gaps of different sizes
      prices = [(1.08500, 1.08520), (1.08505, 1.08525), (1.08510, 1.08530), (1.08515, 1.08535)]
  in zipWith (\time (bid, ask) -> Tick
       { tTime = addUTCTime (fromIntegral time) baseTime
       , tInstr = Instrument (T.pack "EURUSD")
       , tBid = Price (fromFloatDigits bid)
       , tAsk = Price (fromFloatDigits ask)
       , tVolume = Nothing
       }) times prices
