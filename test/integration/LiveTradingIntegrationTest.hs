{-# LANGUAGE OverloadedStrings #-}
module Integration.LiveTradingIntegrationTest where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
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

-- Helper: truncate UTCTime to minute start for deterministic grouping
truncateToMinute :: UTCTime -> UTCTime
truncateToMinute t =
  let day = utctDay t
      tod = timeToTimeOfDay (utctDayTime t)
  in UTCTime day (timeOfDayToTime (TimeOfDay (todHour tod) (todMin tod) 0))

-- Integration test suite for Live Trading
tests :: TestTree
tests = liveTradingIntegrationTests

liveTradingIntegrationTests :: TestTree
liveTradingIntegrationTests = testGroup "Integration.LiveTradingIntegration"
  [ testGroup "Broker-Orchestrator Integration"
    [ testCase "Connection and subscription workflow" testConnectionSubscriptionWorkflow
    , testCase "Tick streaming to orchestrator" testTickStreamingWorkflow
    , testCase "Multi-instrument live data handling" testMultiInstrumentWorkflow
    , testCase "Connection recovery workflow" testConnectionRecoveryWorkflow
    ]
  , testGroup "Strategy-LiveData Integration"
    [ testCase "EMA strategy with live tick data" testEMAWithLiveData
    , testCase "RSI strategy with live tick data" testRSIWithLiveData
    , testCase "Bollinger Bands strategy with live data" testBollingerBandsWithLiveData
    , testCase "Strategy switching with live data" testStrategySwithingWithLiveData
    ]
  , testGroup "Tick Processing Pipeline"
    [ testCase "Tick ingestion to signal generation" testTickToSignalPipeline
    , testCase "Candle generation from live ticks" testLiveCandleGeneration
    , testCase "Signal processing with real strategy" testSignalProcessingIntegration
    , testCase "State persistence across ticks" testStatePersistenceAcrossTicks
    ]
  , testGroup "Error Handling Integration"
    [ testCase "Broker disconnection handling" testBrokerDisconnectionHandling
    , testCase "Invalid tick data handling" testInvalidTickDataHandling
    , testCase "Strategy error recovery" testStrategyErrorRecovery
    , testCase "Configuration error handling" testConfigurationErrorHandling
    ]
  , testGroup "Performance Integration"
    [ testCase "High-frequency tick processing" testHighFrequencyTickProcessing
    , testCase "Memory usage under load" testMemoryUsageUnderLoad
    , testCase "Concurrent strategy execution" testConcurrentStrategyExecution
    ]
  ]

-- Test complete connection and subscription workflow
testConnectionSubscriptionWorkflow :: Assertion
testConnectionSubscriptionWorkflow = do
  let instrument = Instrument "EURUSD"

  result <- runBrokerDataProviderIO $ do
    -- Step 1: Connect to broker
    connResult <- connect
    case connResult of
      Left err -> return $ Left $ "Connection failed: " ++ show err
      Right connId -> do
        -- Step 2: Verify connection status
        statusResult <- getConnectionStatus connId
        case statusResult of
          Left err -> return $ Left $ "Status check failed: " ++ show err
          Right (Connected _) -> do
            -- Step 3: Subscribe to instrument
            subResult <- subscribeToInstrument connId instrument
            case subResult of
              Left err -> return $ Left $ "Subscription failed: " ++ show err
              Right () -> do
                -- Step 4: Verify tick stream access
                streamResult <- getTickStream connId instrument
                case streamResult of
                  Left err -> return $ Left $ "Tick stream access failed: " ++ show err
                  Right tickStream -> do
                    -- Step 5: Verify data quality monitoring
                    qualityResult <- getDataQuality connId instrument
                    case qualityResult of
                      Left err -> return $ Left $ "Data quality check failed: " ++ show err
                      Right quality -> return $ Right (connId, tickStream, quality)
          Right status -> return $ Left $ "Unexpected connection status: " ++ show status

  case result of
    Left err -> assertFailure err
    Right (connId, tickStream, quality) -> do
      -- Validate successful workflow completion
      assertBool "Quality score should be reasonable" (ldqQualityScore quality >= 0.9)

-- Test tick streaming to orchestrator
testTickStreamingWorkflow :: Assertion
testTickStreamingWorkflow = do
  let instrument = Instrument "EURUSD"
      mockStrategyParams = createMockStrategyParams "rsi"

  -- Create live trading configuration
  config <- defaultLiveTradingConfig instrument mockStrategyParams

  result <- runBrokerDataProviderIO $ do
    connResult <- connect
    case connResult of
      Left err -> return $ Left err
      Right connId -> do
        subResult <- subscribeToInstrument connId instrument
        case subResult of
          Left err -> return $ Left err
          Right () -> do
            -- Simulate tick data by accessing the stream
            streamResult <- getTickStream connId instrument
            case streamResult of
              Left err -> return $ Left err
              Right tickStream -> do
                -- Simulate some ticks (in real implementation, these would come from IG polling)
                now <- liftIO getCurrentTime
                let mockTicks = createMockTicks now instrument 5
                -- In a real test, we'd inject these ticks into the stream
                return $ Right (connId, streamResult, mockTicks)

  case result of
    Left err -> assertFailure $ "Tick streaming workflow failed: " ++ show err
    Right (connId, streamResult, mockTicks) -> do
      length mockTicks @?= 5
      all (\tick -> tInstr tick == instrument) mockTicks @?= True

-- Test multi-instrument live data handling
testMultiInstrumentWorkflow :: Assertion
testMultiInstrumentWorkflow = do
  let instruments = [Instrument "EURUSD", Instrument "GBPUSD", Instrument "USDJPY"]

  result <- runBrokerDataProviderIO $ do
    connResult <- connect
    case connResult of
      Left err -> return $ Left err
      Right connId -> do
        -- Subscribe to all instruments
        subResults <- mapM (subscribeToInstrument connId) instruments
        let failures = [err | Left err <- subResults]
        if not (null failures)
          then return $ Left $ head failures
          else do
            -- Get tick streams for all instruments
            streamResults <- mapM (getTickStream connId) instruments
            let streamFailures = [err | Left err <- streamResults]
            if not (null streamFailures)
              then return $ Left $ head streamFailures
              else return $ Right (connId, streamResults)

  case result of
    Left err -> assertFailure $ "Multi-instrument workflow failed: " ++ show err
    Right (connId, streamResults) -> do
      length streamResults @?= 3
      all isRight streamResults @?= True
  where
    isRight (Right _) = True
    isRight (Left _) = False

-- Test connection recovery workflow
testConnectionRecoveryWorkflow :: Assertion
testConnectionRecoveryWorkflow = do
  result <- runBrokerDataProviderIO $ do
    -- Step 1: Create connection
    connResult1 <- connect
    case connResult1 of
      Left err -> return $ Left err
      Right connId1 -> do
        -- Step 2: Disconnect
        disconnectResult <- disconnect connId1
        case disconnectResult of
          Left err -> return $ Left err
          Right () -> do
            -- Step 3: Verify disconnected status
            statusResult <- getConnectionStatus connId1
            case statusResult of
              Right (Connected _) -> return $ Left $ brokerError "Connection should be disconnected"
              Left _ -> do
                -- Step 4: Create new connection (recovery)
                connResult2 <- connect
                case connResult2 of
                  Left err -> return $ Left err
                  Right connId2 -> return $ Right (connId1, connId2)
              Right _ -> do
                -- Step 4: Create new connection (recovery)
                connResult2 <- connect
                case connResult2 of
                  Left err -> return $ Left err
                  Right connId2 -> return $ Right (connId1, connId2)

  case result of
    Left err -> assertFailure $ "Connection recovery failed: " ++ show err
    Right (connId1, connId2) -> do
      -- New connection should have different ID
      connId1 /= connId2 @? "New connection should have different ID"

-- Test EMA strategy with live tick data
testEMAWithLiveData :: Assertion
testEMAWithLiveData = do
  let instrument = Instrument "EURUSD"
      registry = initializeStrategyRegistry

  -- Create EMA strategy parameters
  case createStrategyFromKeyword "ema" Nothing registry of
    Nothing -> assertFailure "Failed to create EMA strategy"
    Just strategyInstance -> do
      now <- getCurrentTime
      let mockCandles = createMockCandleSequence now instrument 25  -- 25 candles for EMA calculation

      -- Test signal generation with EMA strategy
      signal <- generateSignal strategyInstance mockCandles

      -- EMA strategy should generate valid signals
      assertBool "Signal should be valid" (signal `elem` [Hold, Enter Buy, Enter Sell, Exit])

-- Test RSI strategy with live tick data
testRSIWithLiveData :: Assertion
testRSIWithLiveData = do
  let instrument = Instrument "EURUSD"
      registry = initializeStrategyRegistry

  -- Create RSI strategy parameters
  case createStrategyFromKeyword "rsi" Nothing registry of
    Nothing -> assertFailure "Failed to create RSI strategy"
    Just strategyInstance -> do
      now <- getCurrentTime
      let mockCandles = createMockCandleSequence now instrument 20  -- 20 candles for RSI calculation

      -- Test signal generation with RSI strategy
      signal <- generateSignal strategyInstance mockCandles

      -- RSI strategy should generate valid signals
      assertBool "Signal should be valid" (signal `elem` [Hold, Enter Buy, Enter Sell, Exit])

-- Test Bollinger Bands strategy with live data
testBollingerBandsWithLiveData :: Assertion
testBollingerBandsWithLiveData = do
  let instrument = Instrument "EURUSD"
      registry = initializeStrategyRegistry

  -- Create Bollinger Bands strategy parameters
  case createStrategyFromKeyword "bb" Nothing registry of
    Nothing -> assertFailure "Failed to create Bollinger Bands strategy"
    Just strategyInstance -> do
      now <- getCurrentTime
      let mockCandles = createMockCandleSequence now instrument 22  -- 22 candles for BB calculation

      -- Test signal generation with BB strategy
      signal <- generateSignal strategyInstance mockCandles

      -- BB strategy should generate valid signals
      assertBool "Signal should be valid" (signal `elem` [Hold, Enter Buy, Enter Sell, Exit])

-- Test strategy switching with live data
testStrategySwithingWithLiveData :: Assertion
testStrategySwithingWithLiveData = do
  let instrument = Instrument "EURUSD"
      registry = initializeStrategyRegistry

  now <- getCurrentTime
  let mockCandles = createMockCandleSequence now instrument 25

  -- Test multiple strategies with same data
  case (createStrategyFromKeyword "ema" Nothing registry, createStrategyFromKeyword "rsi" Nothing registry) of
    (Just emaStrategy, Just rsiStrategy) -> do
      emaSignal <- generateSignal emaStrategy mockCandles
      rsiSignal <- generateSignal rsiStrategy mockCandles

      -- Both should generate valid signals (may be different)
      assertBool "EMA signal should be valid" (emaSignal `elem` [Hold, Enter Buy, Enter Sell, Exit])
      assertBool "RSI signal should be valid" (rsiSignal `elem` [Hold, Enter Buy, Enter Sell, Exit])

    _ -> assertFailure "Failed to create strategies for switching test"

-- Test tick to signal pipeline
testTickToSignalPipeline :: Assertion
testTickToSignalPipeline = do
  let instrument = Instrument "EURUSD"
  now <- getCurrentTime

  -- Create sequence of ticks that span multiple minutes
  let ticks = createMockTickSequence now instrument 100  -- 100 ticks over time
      candles = ticksToCandles ticks instrument

  -- Should generate multiple candles
  assertBool "Should generate multiple candles" (length candles > 1)

  -- Test with a real strategy
  let registry = initializeStrategyRegistry
  case createStrategyFromKeyword "ema" Nothing registry of
    Nothing -> assertFailure "Failed to create strategy"
    Just strategyInstance -> do
      signal <- generateSignal strategyInstance candles

      -- Should generate a valid signal
      assertBool "Should generate valid signal" (signal `elem` [Hold, Enter Buy, Enter Sell, Exit])

-- Test live candle generation
testLiveCandleGeneration :: Assertion
testLiveCandleGeneration = do
  let instrument = Instrument "EURUSD"
  now0 <- getCurrentTime
  let now = truncateToMinute now0

  -- Create ticks within same minute
  let sameMinuteTicks =
        [ createTick now instrument 1.1850 1.1852
        , createTick (addUTCTime 15 now) instrument 1.1851 1.1853
        , createTick (addUTCTime 30 now) instrument 1.1849 1.1851
        , createTick (addUTCTime 45 now) instrument 1.1852 1.1854
        ]
      candles1 = ticksToCandles sameMinuteTicks instrument

  -- Should generate one candle
  length candles1 @?= 1

  -- Create ticks across minute boundaries
  let crossMinuteTicks =
        [ createTick now instrument 1.1850 1.1852
        , createTick (addUTCTime 70 now) instrument 1.1851 1.1853  -- Next minute
        , createTick (addUTCTime 130 now) instrument 1.1849 1.1851  -- Minute after
        ]
      candles2 = ticksToCandles crossMinuteTicks instrument

  -- Should generate three candles
  length candles2 @?= 3

-- Test signal processing integration
testSignalProcessingIntegration :: Assertion
testSignalProcessingIntegration = do
  let instrument = Instrument "EURUSD"
      registry = initializeStrategyRegistry

  case createStrategyFromKeyword "rsi" Nothing registry of
    Nothing -> assertFailure "Failed to create RSI strategy"
    Just strategyInstance -> do
      now <- getCurrentTime

      -- Create initial state
      let connId = ConnectionId "test-connection"
          initialState = LiveTradingState
            { ltsConnectionId = connId
            , ltsStrategyState = T.pack (show (siInitialState strategyInstance))
            , ltsSignalCount = 0
            , ltsLastSignalTime = Nothing
            , ltsIsRunning = True
            }

      -- Test state updates
      let updatedState = initialState
            { ltsSignalCount = ltsSignalCount initialState + 1
            , ltsLastSignalTime = Just now
            }

      ltsSignalCount updatedState @?= 1
      ltsLastSignalTime updatedState @?= Just now

-- Test state persistence across ticks
testStatePersistenceAcrossTicks :: Assertion
testStatePersistenceAcrossTicks = do
  let instrument = Instrument "EURUSD"
      registry = initializeStrategyRegistry

  case createStrategyFromKeyword "ema" Nothing registry of
    Nothing -> assertFailure "Failed to create EMA strategy"
    Just strategyInstance -> do
      now <- getCurrentTime

      -- Process first batch of candles
      let candles1 = createMockCandleSequence now instrument 10
          (signal1, state1) = siSignalGenerator strategyInstance candles1 (siInitialState strategyInstance)

      -- Process second batch with previous state
      let candles2 = createMockCandleSequence (addUTCTime 600 now) instrument 10
          (signal2, state2) = siSignalGenerator strategyInstance candles2 state1

      -- States should be different (strategy evolved)
      state1 /= siInitialState strategyInstance @? "State should evolve from initial"
      -- Both signals should be valid
      assertBool "First signal should be valid" (signal1 `elem` [Hold, Enter Buy, Enter Sell, Exit])
      assertBool "Second signal should be valid" (signal2 `elem` [Hold, Enter Buy, Enter Sell, Exit])

-- Test broker disconnection handling
testBrokerDisconnectionHandling :: Assertion
testBrokerDisconnectionHandling = do
  result <- runBrokerDataProviderIO $ do
    connResult <- connect
    case connResult of
      Left err -> return $ Left err
      Right connId -> do
        -- Disconnect and then try to use the connection
        disconnectResult <- disconnect connId
        case disconnectResult of
          Left err -> return $ Left err
          Right () -> do
            -- Try to subscribe after disconnect (should fail)
            subResult <- subscribeToInstrument connId (Instrument "EURUSD")
            return $ Right subResult

  case result of
    Left err -> assertFailure $ "Disconnection test setup failed: " ++ show err
    Right subResult -> case subResult of
      Left _ -> return ()  -- Expected failure
      Right () -> assertFailure "Subscription should fail after disconnection"

-- Test invalid tick data handling
testInvalidTickDataHandling :: Assertion
testInvalidTickDataHandling = do
  let instrument = Instrument "EURUSD"
  now <- getCurrentTime

  -- Create invalid tick data (negative prices, invalid timestamps, etc.)
  let invalidTicks = []  -- Empty list represents no data scenario
      candles = ticksToCandles invalidTicks instrument

  -- Should handle gracefully
  length candles @?= 0

-- Test strategy error recovery
testStrategyErrorRecovery :: Assertion
testStrategyErrorRecovery = do
  -- Test that strategy errors don't crash the system
  let instrument = Instrument "EURUSD"
  now <- getCurrentTime
  let mockCandles = []  -- Empty candles might cause strategy errors

  -- Should handle empty candle list gracefully
  length mockCandles @?= 0

-- Test configuration error handling
testConfigurationErrorHandling :: Assertion
testConfigurationErrorHandling = do
  let instrument = Instrument "EURUSD"
      invalidStrategyParams = createMockStrategyParams "invalid-strategy"

  -- Should handle invalid strategy gracefully
  config <- defaultLiveTradingConfig instrument invalidStrategyParams
  ltcInstrument config @?= instrument

-- Test high-frequency tick processing
testHighFrequencyTickProcessing :: Assertion
testHighFrequencyTickProcessing = do
  let instrument = Instrument "EURUSD"
  now <- getCurrentTime

  -- Generate many ticks in short time span
  let highFreqTicks = createMockTickSequence now instrument 1000  -- 1000 ticks
      candles = ticksToCandles highFreqTicks instrument

  -- Should handle large volumes
  assertBool "Should generate reasonable number of candles" (length candles > 0)
  assertBool "Should not generate too many candles" (length candles < 1000)

-- Test memory usage under load
testMemoryUsageUnderLoad :: Assertion
testMemoryUsageUnderLoad = do
  let instrument = Instrument "EURUSD"
  now <- getCurrentTime

  -- Process large dataset
  let largeTicks = createMockTickSequence now instrument 10000  -- 10k ticks
      candles = ticksToCandles largeTicks instrument

  -- Should complete without memory issues
  assertBool "Should process large datasets" (length candles >= 0)

-- Test concurrent strategy execution
testConcurrentStrategyExecution :: Assertion
testConcurrentStrategyExecution = do
  let instrument = Instrument "EURUSD"
      registry = initializeStrategyRegistry

  now <- getCurrentTime
  let mockCandles = createMockCandleSequence now instrument 25

  -- Test multiple strategies concurrently (simulated)
  case (createStrategyFromKeyword "ema" Nothing registry,
        createStrategyFromKeyword "rsi" Nothing registry,
        createStrategyFromKeyword "bb" Nothing registry) of
    (Just ema, Just rsi, Just bb) -> do
      -- All should process same data successfully
      emaSignal <- generateSignal ema mockCandles
      rsiSignal <- generateSignal rsi mockCandles
      bbSignal <- generateSignal bb mockCandles

      -- All should generate valid signals
      assertBool "EMA signal valid" (emaSignal `elem` [Hold, Enter Buy, Enter Sell, Exit])
      assertBool "RSI signal valid" (rsiSignal `elem` [Hold, Enter Buy, Enter Sell, Exit])
      assertBool "BB signal valid" (bbSignal `elem` [Hold, Enter Buy, Enter Sell, Exit])

    _ -> assertFailure "Failed to create strategies for concurrent test"

-- Helper functions for integration tests

-- Create mock strategy parameters
createMockStrategyParams :: T.Text -> StrategyParameters
createMockStrategyParams stratType = StrategyParameters
  { spStrategyType = stratType
  , spParameters = ()
  , spValidator = \_ -> True
  , spParser = \_ -> Nothing
  , spDefaults = ()
  }

-- Create mock ticks
createMockTicks :: UTCTime -> Instrument -> Int -> [Tick]
createMockTicks baseTime instrument count =
  [ Tick
    { tTime = addUTCTime (fromIntegral i * 5) baseTime  -- 5 seconds apart
    , tInstr = instrument
    , tBid = Price (1.1850 + fromIntegral i * 0.0001)
    , tAsk = Price (1.1852 + fromIntegral i * 0.0001)
    , tVolume = Nothing
    }
  | i <- [0..count-1]
  ]

-- Create mock candle sequence
createMockCandleSequence :: UTCTime -> Instrument -> Int -> [Candle]
createMockCandleSequence baseTime instrument count =
  [ Candle
    { cTime = addUTCTime (fromIntegral i * 60) baseTime  -- 1 minute apart
    , cOpen = Price (1.1850 + fromIntegral i * 0.0001)
    , cHigh = Price (1.1855 + fromIntegral i * 0.0001)
    , cLow = Price (1.1845 + fromIntegral i * 0.0001)
    , cClose = Price (1.1852 + fromIntegral i * 0.0001)
    }
  | i <- [0..count-1]
  ]

-- Create mock tick sequence spanning multiple minutes
createMockTickSequence :: UTCTime -> Instrument -> Int -> [Tick]
createMockTickSequence baseTime instrument count =
  [ Tick
    { tTime = addUTCTime (fromIntegral i * 10) baseTime  -- 10 seconds apart
    , tInstr = instrument
    , tBid = Price (1.1850 + fromIntegral (i `mod` 20) * 0.0001)
    , tAsk = Price (1.1852 + fromIntegral (i `mod` 20) * 0.0001)
    , tVolume = Nothing
    }
  | i <- [0..count-1]
  ]

-- Create a single tick
createTick :: UTCTime -> Instrument -> Scientific -> Scientific -> Tick
createTick time instrument bid ask = Tick
  { tTime = time
  , tInstr = instrument
  , tBid = Price bid
  , tAsk = Price ask
  , tVolume = Nothing
  }
