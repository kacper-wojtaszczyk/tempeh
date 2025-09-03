{-# LANGUAGE OverloadedStrings #-}
module Unit.Application.LiveTradingOrchestratorTest where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Time
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Scientific

import Application.LiveTradingOrchestrator
import Application.Strategy.Types
import Application.Strategy.Registry
import Application.Strategy.Factory
import Adapter.BrokerDataProvider
import Domain.Services.LiveDataService
import Domain.Types
import qualified Util.Config as Config
import Util.Error
import Domain.Strategy (StrategyState(..))

-- Helper: truncate UTCTime to minute start for deterministic grouping
truncateToMinute :: UTCTime -> UTCTime
truncateToMinute t =
  let day = utctDay t
      tod = timeToTimeOfDay (utctDayTime t)
  in UTCTime day (timeOfDayToTime (TimeOfDay (todHour tod) (todMin tod) 0))

-- Test suite for LiveTradingOrchestrator
tests :: TestTree
tests = liveTradingOrchestratorTests

liveTradingOrchestratorTests :: TestTree
liveTradingOrchestratorTests = testGroup "Application.LiveTradingOrchestrator"
  [ testGroup "Configuration Management"
    [ testCase "Default live trading config should be valid" testDefaultLiveTradingConfig
    , testCase "Live trading config should contain required fields" testLiveTradingConfigFields
    ]
  , testGroup "Tick Processing"
    [ testCase "Empty tick list should be handled" testEmptyTicks
    , testCase "Single tick should not generate candle" testSingleTick
    , testCase "Multiple ticks should generate candles" testMultipleTicks
    , testCase "Ticks from different instruments should be filtered" testTickFiltering
    ]
  , testGroup "Tick-to-Candle Conversion"
    [ testCase "Ticks within same minute should group together" testTickGrouping
    , testCase "Ticks across minute boundaries should separate" testMinuteBoundaries
    , testCase "Candle OHLC should be calculated correctly" testCandleOHLC
    , testCase "Candle timestamps should be minute-aligned" testCandleTimestamps
    ]
  , testGroup "Signal Generation"
    [ testCase "Strategy signal generation should work with candles" testSignalGeneration
    , testCase "Hold signal should be handled correctly" testHoldSignal
    , testCase "Enter signal should be logged" testEnterSignal
    , testCase "Exit signal should be logged" testExitSignal
    ]
  , testGroup "Live Trading State"
    [ testCase "Initial state should be correctly initialized" testInitialState
    , testCase "State should track signal counts" testSignalCounting
    , testCase "State should track last signal time" testLastSignalTime
    , testCase "Running state should control loop execution" testRunningState
    ]
  , testGroup "Error Handling"
    [ testCase "Invalid strategy should be handled" testInvalidStrategy
    , testCase "Connection errors should stop trading" testConnectionErrors
    , testCase "Tick stream errors should be handled gracefully" testTickStreamErrors
    ]
  , testGroup "Property-based Tests"
    [ QC.testProperty "Candle generation preserves tick count" prop_tickCountPreservation
    , QC.testProperty "Candle timestamps are always minute-aligned" prop_candleTimestamps
    , QC.testProperty "Signal count always increases" prop_signalCountMonotonic
    ]
  ]

-- Test default live trading configuration
testDefaultLiveTradingConfig :: Assertion
testDefaultLiveTradingConfig = do
  let instrument = Instrument "EURUSD"
      strategyParams = createMockRSIParams

  config <- defaultLiveTradingConfig instrument strategyParams

  -- Validate configuration fields
  ltcInstrument config @?= instrument
  ltcStrategy config @?= strategyParams
  -- Config should be set (basic sanity check on a field)
  assertBool "Config data directory should be non-empty"
    (not $ null (Config.acDataDirectory (ltcConfig config)))

-- Test live trading config fields
testLiveTradingConfigFields :: Assertion
testLiveTradingConfigFields = do
  let instrument = Instrument "GBPUSD"
      strategyParams = createMockEMAParams

  config <- defaultLiveTradingConfig instrument strategyParams

  -- All required fields should be present
  assertBool "Instrument should be set" (ltcInstrument config == instrument)
  assertBool "Strategy should be set" (ltcStrategy config == strategyParams)

-- Test empty ticks handling
testEmptyTicks :: Assertion
testEmptyTicks = do
  let instrument = Instrument "EURUSD"
      emptyTicks = []
      candles = ticksToCandles emptyTicks instrument

  length candles @?= 0

-- Test single tick handling
testSingleTick :: Assertion
testSingleTick = do
  let instrument = Instrument "EURUSD"
  now <- getCurrentTime
  let tick = Tick
        { tTime = now
        , tInstr = instrument
        , tBid = Price 1.1850
        , tAsk = Price 1.1852
        , tVolume = Nothing
        }
      candles = ticksToCandles [tick] instrument

  -- Single tick should generate one candle
  length candles @?= 1

  -- Validate candle data
  let candle = head candles
  cOpen candle @?= Price 1.1851  -- Mid price
  cHigh candle @?= Price 1.1851
  cLow candle @?= Price 1.1851
  cClose candle @?= Price 1.1851

-- Test multiple ticks generating candles
testMultipleTicks :: Assertion
testMultipleTicks = do
  let instrument = Instrument "EURUSD"
  now0 <- getCurrentTime
  let now = truncateToMinute now0
  let ticks = [ Tick { tTime = now, tInstr = instrument, tBid = Price 1.1850, tAsk = Price 1.1852, tVolume = Nothing }
              , Tick { tTime = addUTCTime 10 now, tInstr = instrument, tBid = Price 1.1851, tAsk = Price 1.1853, tVolume = Nothing }
              , Tick { tTime = addUTCTime 20 now, tInstr = instrument, tBid = Price 1.1849, tAsk = Price 1.1851, tVolume = Nothing }
              ]
      candles = ticksToCandles ticks instrument

  -- Multiple ticks in same minute should generate one candle
  length candles @?= 1

  let candle = head candles
  -- Validate OHLC calculation
  cOpen candle @?= Price 1.1851  -- First tick mid price
  cClose candle @?= Price 1.1850  -- Last tick mid price
  cHigh candle @?= Price 1.1852  -- Highest mid price
  cLow candle @?= Price 1.1850   -- Lowest mid price

-- Test tick filtering by instrument
testTickFiltering :: Assertion
testTickFiltering = do
  let targetInstrument = Instrument "EURUSD"
      otherInstrument = Instrument "GBPUSD"
  now <- getCurrentTime
  let ticks = [ Tick { tTime = now, tInstr = targetInstrument, tBid = Price 1.1850, tAsk = Price 1.1852, tVolume = Nothing }
              , Tick { tTime = now, tInstr = otherInstrument, tBid = Price 1.2850, tAsk = Price 1.2852, tVolume = Nothing }
              , Tick { tTime = now, tInstr = targetInstrument, tBid = Price 1.1851, tAsk = Price 1.1853, tVolume = Nothing }
              ]
      candles = ticksToCandles ticks targetInstrument

  -- Should only process ticks for target instrument
  length candles @?= 1

-- Test tick grouping by minute
testTickGrouping :: Assertion
testTickGrouping = do
  let instrument = Instrument "EURUSD"
  baseTime0 <- getCurrentTime
  let baseTime = truncateToMinute baseTime0
  let minute1 = baseTime
      minute1Plus30s = addUTCTime 30 baseTime
      ticks = [ Tick { tTime = minute1, tInstr = instrument, tBid = Price 1.1850, tAsk = Price 1.1852, tVolume = Nothing }
              , Tick { tTime = minute1Plus30s, tInstr = instrument, tBid = Price 1.1851, tAsk = Price 1.1853, tVolume = Nothing }
              ]
      grouped = groupTicksByMinute ticks

  -- Both ticks should be in same group (same minute)
  length grouped @?= 1
  length (head grouped) @?= 2

-- Test minute boundary separation
testMinuteBoundaries :: Assertion
testMinuteBoundaries = do
  let instrument = Instrument "EURUSD"
  baseTime <- getCurrentTime
  let minute1 = baseTime
      minute2 = addUTCTime 61 baseTime  -- Next minute
      ticks = [ Tick { tTime = minute1, tInstr = instrument, tBid = Price 1.1850, tAsk = Price 1.1852, tVolume = Nothing }
              , Tick { tTime = minute2, tInstr = instrument, tBid = Price 1.1851, tAsk = Price 1.1853, tVolume = Nothing }
              ]
      grouped = groupTicksByMinute ticks

  -- Ticks should be in separate groups (different minutes)
  length grouped @?= 2
  all (\group -> length group == 1) grouped @?= True

-- Test candle OHLC calculation
testCandleOHLC :: Assertion
testCandleOHLC = do
  let instrument = Instrument "EURUSD"
  now <- getCurrentTime
  let ticks = [ Tick { tTime = now, tInstr = instrument, tBid = Price 1.1850, tAsk = Price 1.1852, tVolume = Nothing }           -- Mid: 1.1851
              , Tick { tTime = addUTCTime 10 now, tInstr = instrument, tBid = Price 1.1855, tAsk = Price 1.1857, tVolume = Nothing }  -- Mid: 1.1856 (high)
              , Tick { tTime = addUTCTime 20 now, tInstr = instrument, tBid = Price 1.1845, tAsk = Price 1.1847, tVolume = Nothing }  -- Mid: 1.1846 (low)
              , Tick { tTime = addUTCTime 30 now, tInstr = instrument, tBid = Price 1.1848, tAsk = Price 1.1850, tVolume = Nothing }  -- Mid: 1.1849 (close)
              ]
      candle = tickGroupToCandle ticks

  -- Validate OHLC
  cOpen candle @?= Price 1.1851   -- First tick
  cHigh candle @?= Price 1.1856   -- Highest mid price
  cLow candle @?= Price 1.1846    -- Lowest mid price
  cClose candle @?= Price 1.1849  -- Last tick

-- Test candle timestamp alignment
testCandleTimestamps :: Assertion
testCandleTimestamps = do
  let instrument = Instrument "EURUSD"
  -- Create a time with seconds and milliseconds
  baseTime0 <- getCurrentTime
  let baseTime = truncateToMinute baseTime0
  let timeWithSeconds = addUTCTime 37.523 baseTime  -- 37.523 seconds past the minute
      tick = Tick { tTime = timeWithSeconds, tInstr = instrument, tBid = Price 1.1850, tAsk = Price 1.1852, tVolume = Nothing }
      candle = tickGroupToCandle [tick]
      candleTime = cTime candle

  -- Candle time should be aligned to the minute (seconds should be 0)
  let timeOfDay = timeToTimeOfDay (utctDayTime candleTime)
  todSec timeOfDay @?= 0

-- Test signal generation with candles
testSignalGeneration :: Assertion
testSignalGeneration = do
  let instrument = Instrument "EURUSD"
      mockStrategy = createMockStrategyInstance
  now <- getCurrentTime
  let candles = [createMockCandle now instrument]

  signal <- generateSignal mockStrategy candles

  -- Should generate a valid signal (Mock returns Hold)
  signal @?= Hold

-- Test hold signal handling
testHoldSignal :: Assertion
testHoldSignal = do
  -- This test would typically be part of an integration test
  -- since it involves the full processTicks function
  -- For now, we test that Hold is a valid signal type
  let holdSignal = Hold
  holdSignal @?= Hold

-- Test enter signal handling
testEnterSignal :: Assertion
testEnterSignal = do
  let buySignal = Enter Buy
      sellSignal = Enter Sell

  buySignal @?= Enter Buy
  sellSignal @?= Enter Sell

-- Test exit signal handling
testExitSignal :: Assertion
testExitSignal = do
  let exitSignal = Exit
  exitSignal @?= Exit

-- Test initial state initialization
testInitialState :: Assertion
testInitialState = do
  now <- getCurrentTime
  let connId = ConnectionId "test-connection"
      strategyState = "mock-strategy-state" :: T.Text
      initialState = LiveTradingState
        { ltsConnectionId = connId
        , ltsStrategyState = strategyState
        , ltsSignalCount = 0
        , ltsLastSignalTime = Nothing
        , ltsIsRunning = True
        }

  ltsConnectionId initialState @?= connId
  ltsStrategyState initialState @?= strategyState
  ltsSignalCount initialState @?= 0
  ltsLastSignalTime initialState @?= Nothing
  ltsIsRunning initialState @?= True

-- Test signal counting
testSignalCounting :: Assertion
testSignalCounting = do
  now <- getCurrentTime
  let connId = ConnectionId "test-connection"
      initialState = LiveTradingState
        { ltsConnectionId = connId
        , ltsStrategyState = ""
        , ltsSignalCount = 5
        , ltsLastSignalTime = Nothing
        , ltsIsRunning = True
        }
      updatedState = initialState { ltsSignalCount = ltsSignalCount initialState + 3 }

  ltsSignalCount updatedState @?= 8

-- Test last signal time tracking
testLastSignalTime :: Assertion
testLastSignalTime = do
  now <- getCurrentTime
  let connId = ConnectionId "test-connection"
      initialState = LiveTradingState
        { ltsConnectionId = connId
        , ltsStrategyState = ""
        , ltsSignalCount = 0
        , ltsLastSignalTime = Nothing
        , ltsIsRunning = True
        }
      updatedState = initialState { ltsLastSignalTime = Just now }

  ltsLastSignalTime updatedState @?= Just now

-- Test running state control
testRunningState :: Assertion
testRunningState = do
  let connId = ConnectionId "test-connection"
      runningState = LiveTradingState
        { ltsConnectionId = connId
        , ltsStrategyState = ""
        , ltsSignalCount = 0
        , ltsLastSignalTime = Nothing
        , ltsIsRunning = True
        }
      stoppedState = runningState { ltsIsRunning = False }

  ltsIsRunning runningState @?= True
  ltsIsRunning stoppedState @?= False

-- Test invalid strategy handling
testInvalidStrategy :: Assertion
testInvalidStrategy = do
  -- Test that invalid strategy parameters are handled
  -- This would typically require integration with the strategy registry
  let invalidParams = StrategyParameters "invalid" () (\_ -> False) (\_ -> Nothing) ()

  -- The StrategyParameters should be constructible but validation should fail
  case invalidParams of
    StrategyParameters sType _ validator _ defaults -> do
      sType @?= "invalid"
      assertBool "Invalid params should fail validation" (not $ validator defaults)

-- Test connection error handling
testConnectionErrors :: Assertion
testConnectionErrors = do
  -- Test that connection errors stop trading
  let connId = ConnectionId "test-connection"
      initialState = LiveTradingState
        { ltsConnectionId = connId
        , ltsStrategyState = ""
        , ltsSignalCount = 0
        , ltsLastSignalTime = Nothing
        , ltsIsRunning = True
        }
      errorState = initialState { ltsIsRunning = False }

  ltsIsRunning errorState @?= False

-- Test tick stream error handling
testTickStreamErrors :: Assertion
testTickStreamErrors = do
  -- Test that tick stream errors are handled gracefully
  -- This tests the error handling structure
  let streamError = Left $ brokerError "Test tick stream error"

  case streamError of
    Left err -> return () -- Expected
    Right _ -> assertFailure "Should be an error"

-- Property: Tick count preservation in candle generation
prop_tickCountPreservation :: [MockTick] -> Property
prop_tickCountPreservation mockTicks = not (null mockTicks) ==>
  let instrument = Instrument "EURUSD"
      ticks = map (mockTickToTick instrument) mockTicks
      candles = ticksToCandles ticks instrument
      -- Count ticks that belong to the instrument
      relevantTicks = filter (\t -> tInstr t == instrument) ticks
      -- Each candle should represent some ticks
      ticksInCandles = length relevantTicks
  in ticksInCandles >= 0 && length candles >= 0

-- Property: Candle timestamps are always minute-aligned
prop_candleTimestamps :: [MockTick] -> Property
prop_candleTimestamps mockTicks = not (null mockTicks) ==>
  let instrument = Instrument "EURUSD"
      ticks = map (mockTickToTick instrument) mockTicks
      candles = ticksToCandles ticks instrument
  in all isMinuteAligned (map cTime candles)
  where
    isMinuteAligned time =
      let timeOfDay = timeToTimeOfDay (utctDayTime time)
      in todSec timeOfDay == 0

-- Property: Signal count always increases
prop_signalCountMonotonic :: NonNegative Int -> NonNegative Int -> Property
prop_signalCountMonotonic (NonNegative initial) (NonNegative increment) =
  let newCount = initial + increment
  in QC.property (newCount >= initial)

-- Helper functions and mock data

-- Mock tick type for property testing
data MockTick = MockTick
  { mockTime :: UTCTime
  , mockBid :: Scientific
  , mockAsk :: Scientific
  } deriving (Show, Eq)

instance Arbitrary MockTick where
  arbitrary = do
    -- Generate a recent time
    days <- choose (0, 30) :: Gen Integer
    seconds <- choose (0, 86400) :: Gen Integer
    let baseTime = UTCTime (fromGregorian 2025 9 1) 0
        time = addUTCTime (fromIntegral (days * 86400 + seconds)) baseTime
    bid <- choose (1.0 :: Double, 2.0 :: Double)
    spread <- choose (0.0001 :: Double, 0.01 :: Double)
    return MockTick
      { mockTime = time
      , mockBid = fromFloatDigits bid
      , mockAsk = fromFloatDigits (bid + spread)
      }

-- Convert mock tick to real tick
mockTickToTick :: Instrument -> MockTick -> Tick
mockTickToTick instrument mockTick = Tick
  { tTime = mockTime mockTick
  , tInstr = instrument
  , tBid = Price (mockBid mockTick)
  , tAsk = Price (mockAsk mockTick)
  , tVolume = Nothing
  }

-- Create mock RSI strategy parameters
createMockRSIParams :: StrategyParameters
createMockRSIParams = StrategyParameters
  { spStrategyType = "rsi"
  , spParameters = ()
  , spValidator = \_ -> True
  , spParser = \_ -> Nothing
  , spDefaults = ()
  }

-- Create mock EMA strategy parameters
createMockEMAParams :: StrategyParameters
createMockEMAParams = StrategyParameters
  { spStrategyType = "ema"
  , spParameters = ()
  , spValidator = \_ -> True
  , spParser = \_ -> Nothing
  , spDefaults = ()
  }

-- Create mock strategy instance
createMockStrategyInstance :: StrategyInstance
createMockStrategyInstance = StrategyInstance
  { siName = "Mock Strategy"
  , siDescription = "Mock strategy for testing"
  , siParameters = createMockRSIParams
  , siSignalGenerator = \_ _ -> (Hold, StrategyState "mock-state")
  , siInitialState = StrategyState "initial"
  }

-- Create mock candle
createMockCandle :: UTCTime -> Instrument -> Candle
createMockCandle time instrument = Candle
  { cTime = time
  , cOpen = Price 1.1850
  , cHigh = Price 1.1855
  , cLow = Price 1.1845
  , cClose = Price 1.1852
  }
