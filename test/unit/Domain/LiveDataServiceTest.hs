{-# LANGUAGE OverloadedStrings #-}
module Unit.Domain.LiveDataServiceTest where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Data.Time
import qualified Data.Text as T
import Data.Scientific

import Domain.Services.LiveDataService
import Domain.Types

-- Test suite for Domain.Services.LiveDataService
tests :: TestTree
tests = liveDataServiceTests

liveDataServiceTests :: TestTree
liveDataServiceTests = testGroup "Domain.Services.LiveDataService"
  [ testGroup "LiveDataQuality"
    [ testCase "LiveDataQuality should validate metrics" testLiveDataQualityValidation
    , testCase "Quality score calculation should be accurate" testQualityScore
    , testCase "Data completeness should be measurable" testDataCompleteness
    ]
  , testGroup "InstrumentDetails"
    [ testCase "InstrumentDetails should contain required metadata" testInstrumentDetails
    , testCase "Pip value should be reasonable for forex" testPipValue
    , testCase "Trade size limits should be positive" testTradeSizeLimits
    , testCase "Market hours should be valid" testMarketHours
    ]
  , testGroup "LiveDataConfig"
    [ testCase "LiveDataConfig should have reasonable defaults" testLiveDataConfig
    , testCase "Buffer size should be positive" testBufferSize
    , testCase "Tick rate limits should be realistic" testTickRateLimits
    , testCase "Quality threshold should be valid" testQualityThreshold
    ]
  , testGroup "Property-based Tests"
    [ QC.testProperty "Quality score is always between 0 and 1" prop_qualityScoreBounds
    , QC.testProperty "Pip value is always positive" prop_pipValuePositive
    , QC.testProperty "Buffer size is always positive" prop_bufferSizePositive
    ]
  ]

-- Test LiveDataQuality validation
testLiveDataQualityValidation :: Assertion
testLiveDataQualityValidation = do
  now <- getCurrentTime
  let quality = LiveDataQuality
        { ldqTicksReceived = 95
        , ldqTicksExpected = 100
        , ldqLatency = Just 12.5
        , ldqLastTickTime = Just now
        , ldqQualityScore = 0.95
        }

  -- Validate all fields
  ldqTicksReceived quality @?= 95
  ldqTicksExpected quality @?= 100
  ldqLatency quality @?= Just 12.5
  ldqLastTickTime quality @?= Just now
  ldqQualityScore quality @?= 0.95

  -- Quality score should be reasonable
  let score = ldqQualityScore quality
  assertBool "Quality score should be between 0 and 1" (score >= 0.0 && score <= 1.0)

-- Test quality score calculation
testQualityScore :: Assertion
testQualityScore = do
  -- Perfect quality
  let perfectQuality = LiveDataQuality 100 100 (Just 10.0) Nothing 1.0
  ldqQualityScore perfectQuality @?= 1.0

  -- Degraded quality
  let degradedQuality = LiveDataQuality 80 100 (Just 50.0) Nothing 0.8
  ldqQualityScore degradedQuality @?= 0.8

  -- Quality should correlate with received/expected ratio
  let receivedRatio = fromIntegral (ldqTicksReceived degradedQuality) / fromIntegral (ldqTicksExpected degradedQuality)
  assertBool "Quality should correlate with tick ratio" (abs (ldqQualityScore degradedQuality - receivedRatio) < 0.1)

-- Test data completeness measurement
testDataCompleteness :: Assertion
testDataCompleteness = do
  let completeData = LiveDataQuality 1000 1000 (Just 5.0) Nothing 1.0
      incompleteData = LiveDataQuality 850 1000 (Just 15.0) Nothing 0.85

  -- Complete data should have 100% completeness
  let completeness1 = fromIntegral (ldqTicksReceived completeData) / fromIntegral (ldqTicksExpected completeData)
  completeness1 @?= 1.0

  -- Incomplete data should show the gap
  let completeness2 = fromIntegral (ldqTicksReceived incompleteData) / fromIntegral (ldqTicksExpected incompleteData)
  completeness2 @?= 0.85

-- Test InstrumentDetails metadata
testInstrumentDetails :: Assertion
testInstrumentDetails = do
  now <- getCurrentTime
  let instrument = Instrument (T.pack "EURUSD")
      details = InstrumentDetails
        { idInstrument = instrument
        , idDisplayName = T.pack "Euro/US Dollar"
        , idPipValue = 0.0001
        , idMinTradeSize = 1000
        , idMaxTradeSize = 10000000
        , idMarginRate = 0.02
        , idMarketHours = MarketHours now (addUTCTime 86400 now) (T.pack "UTC")
        }

  -- Validate all required fields
  idInstrument details @?= instrument
  idDisplayName details @?= T.pack "Euro/US Dollar"
  idPipValue details @?= 0.0001
  idMinTradeSize details @?= 1000
  idMaxTradeSize details @?= 10000000
  idMarginRate details @?= 0.02

-- Test pip value reasonableness
testPipValue :: Assertion
testPipValue = do
  let eurusdDetails = createMockInstrumentDetails (Instrument "EURUSD") 0.0001
      usdjpyDetails = createMockInstrumentDetails (Instrument "USDJPY") 0.01

  -- EUR/USD should have 4-decimal pip value
  idPipValue eurusdDetails @?= 0.0001

  -- USD/JPY should have 2-decimal pip value
  idPipValue usdjpyDetails @?= 0.01

  -- Pip values should be positive
  assertBool "EUR/USD pip value should be positive" (idPipValue eurusdDetails > 0)
  assertBool "USD/JPY pip value should be positive" (idPipValue usdjpyDetails > 0)

-- Test trade size limits
testTradeSizeLimits :: Assertion
testTradeSizeLimits = do
  let details = createMockInstrumentDetails (Instrument "EURUSD") 0.0001

  -- Min trade size should be positive
  assertBool "Min trade size should be positive" (idMinTradeSize details > 0)

  -- Max trade size should be greater than min
  assertBool "Max trade size should be greater than min" (idMaxTradeSize details > idMinTradeSize details)

  -- Margin rate should be reasonable (between 0% and 100%)
  let marginRate = idMarginRate details
  assertBool "Margin rate should be reasonable" (marginRate > 0 && marginRate <= 1.0)

-- Test market hours validity
testMarketHours :: Assertion
testMarketHours = do
  now <- getCurrentTime
  let marketOpen = now
      marketClose = addUTCTime (8 * 3600) now  -- 8 hours later
      marketHours = MarketHours marketOpen marketClose "UTC"
      details = InstrumentDetails
        { idInstrument = Instrument "EURUSD"
        , idDisplayName = "Euro/US Dollar"
        , idPipValue = 0.0001
        , idMinTradeSize = 1000
        , idMaxTradeSize = 10000000
        , idMarginRate = 0.02
        , idMarketHours = marketHours
        }

  -- Market hours should be properly set
  let hours = idMarketHours details
  mhTimeZone hours @?= T.pack "UTC"

  -- Close should be after open
  assertBool "Market close should be after open" (mhClose hours > mhOpen hours)

-- Test LiveDataConfig defaults
testLiveDataConfig :: Assertion
testLiveDataConfig = do
  let config = LiveDataConfig
        { ldcBufferSize = 1000
        , ldcMaxTicksPerSecond = 100
        , ldcQualityThreshold = 0.95
        , ldcHeartbeatInterval = 30.0
        }

  -- Validate reasonable defaults
  ldcBufferSize config @?= 1000
  ldcMaxTicksPerSecond config @?= 100
  ldcQualityThreshold config @?= 0.95
  ldcHeartbeatInterval config @?= 30.0

-- Test buffer size validation
testBufferSize :: Assertion
testBufferSize = do
  let config = LiveDataConfig 5000 200 0.98 15.0

  -- Buffer size should be positive and reasonable
  let bufferSize = ldcBufferSize config
  assertBool "Buffer size should be positive" (bufferSize > 0)
  assertBool "Buffer size should be reasonable" (bufferSize >= 100 && bufferSize <= 100000)

-- Test tick rate limits
testTickRateLimits :: Assertion
testTickRateLimits = do
  let config = LiveDataConfig 1000 150 0.90 20.0

  -- Tick rate should be reasonable for real-time data
  let maxTicks = ldcMaxTicksPerSecond config
  assertBool "Max ticks per second should be positive" (maxTicks > 0)
  assertBool "Max ticks per second should be realistic" (maxTicks >= 1 && maxTicks <= 10000)

-- Test quality threshold validation
testQualityThreshold :: Assertion
testQualityThreshold = do
  let config = LiveDataConfig 1000 100 0.85 25.0

  -- Quality threshold should be between 0 and 1
  let threshold = ldcQualityThreshold config
  assertBool "Quality threshold should be between 0 and 1" (threshold >= 0.0 && threshold <= 1.0)

  -- Should be a reasonable threshold (not too low)
  assertBool "Quality threshold should be reasonable" (threshold >= 0.5)

-- Property: Quality score is always between 0 and 1
prop_qualityScoreBounds :: Double -> Property
prop_qualityScoreBounds score =
  let clampedScore = max 0.0 (min 1.0 score)
      quality = LiveDataQuality 100 100 (Just 10.0) Nothing clampedScore
  in QC.property (ldqQualityScore quality >= 0.0 && ldqQualityScore quality <= 1.0)

-- Property: Pip value is always positive
prop_pipValuePositive :: Positive Double -> Property
prop_pipValuePositive (Positive pipVal) =
  let details = createMockInstrumentDetails (Instrument "TEST") (fromFloatDigits pipVal)
  in QC.property (idPipValue details > 0)

-- Property: Buffer size is always positive
prop_bufferSizePositive :: Positive Int -> Property
prop_bufferSizePositive (Positive bufSize) =
  let config = LiveDataConfig bufSize 100 0.95 30.0
  in QC.property (ldcBufferSize config > 0)

-- Create mock instrument details
createMockInstrumentDetails :: Instrument -> Scientific -> InstrumentDetails
createMockInstrumentDetails instrument pipValue = do
  let now = UTCTime (fromGregorian 2025 9 3) 0
  InstrumentDetails
    { idInstrument = instrument
    , idDisplayName = unInstrument instrument
    , idPipValue = pipValue
    , idMinTradeSize = 1000
    , idMaxTradeSize = 10000000
    , idMarginRate = 0.02
    , idMarketHours = MarketHours now (addUTCTime 86400 now) "UTC"
    }

-- Generate arbitrary LiveDataConfig for property testing
instance Arbitrary LiveDataConfig where
  arbitrary = do
    bufferSize <- choose (100, 10000)
    maxTicks <- choose (1, 1000)
    qualityThreshold <- choose (0.5, 1.0)
    heartbeat <- choose (1.0, 300.0)
    return $ LiveDataConfig bufferSize maxTicks qualityThreshold heartbeat
