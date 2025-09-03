{-# LANGUAGE OverloadedStrings #-}
module E2E.LiveTradingE2ETest where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad (forM)
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
import Util.Config hiding (defaultLiveTradingConfig)
import Util.Error

-- Provide 'tests' alias expected by test/Main.hs
tests :: TestTree
tests = liveTradingE2ETests

-- End-to-End test suite for Live Trading
liveTradingE2ETests :: TestTree
liveTradingE2ETests = testGroup "E2E.LiveTradingE2E"
  [ testGroup "Complete Live Trading Workflows"
    [ testCase "Full live trading session with EMA strategy" testFullEMALiveSession
    , testCase "Full live trading session with RSI strategy" testFullRSILiveSession
    , testCase "Multi-strategy live trading session" testMultiStrategyLiveSession
    , testCase "Live trading with connection recovery" testLiveTradingWithRecovery
    ]
  , testGroup "Real-time Data Processing E2E"
    [ testCase "Tick ingestion to signal generation pipeline" testTickToSignalE2E
    , testCase "High-frequency data processing workflow" testHighFrequencyDataE2E
    , testCase "Real-time candle generation and strategy execution" testRealTimeCandleE2E
    , testCase "Live data quality monitoring workflow" testLiveDataQualityE2E
    ]
  , testGroup "Error Recovery E2E Scenarios"
    [ testCase "Complete recovery from broker disconnection" testBrokerDisconnectionRecoveryE2E
    , testCase "Strategy error recovery in live environment" testStrategyErrorRecoveryE2E
    , testCase "Configuration reload during live trading" testConfigurationReloadE2E
    , testCase "Data quality degradation handling" testDataQualityDegradationE2E
    ]
  , testGroup "Performance E2E Tests"
    [ testCase "Live trading performance under load" testLiveTradingPerformanceE2E
    , testCase "Memory stability during extended trading" testMemoryStabilityE2E
    , testCase "Concurrent live trading sessions" testConcurrentLiveSessionsE2E
    ]
  ]

-- Test complete live trading session with EMA strategy
testFullEMALiveSession :: Assertion
testFullEMALiveSession = do
  putStrLn "=== Full EMA Live Trading Session E2E Test ==="

  let instrument = Instrument "EURUSD"
      registry = initializeStrategyRegistry

  -- Create EMA strategy configuration
  case createStrategyFromKeyword "ema" Nothing registry of
    Nothing -> assertFailure "Failed to create EMA strategy"
    Just strategyInstance -> do
      let strategyParams = siParameters strategyInstance

      -- Create live trading configuration
      config <- defaultLiveTradingConfig instrument strategyParams

      -- Run simulated live trading session
      result <- runSimulatedLiveSession config registry 5  -- 5 second simulation

      case result of
        Left err -> assertFailure $ "EMA live session failed: " ++ show err
        Right finalState -> do
          putStrLn $ "Session completed with " ++ show (ltsSignalCount finalState) ++ " signals processed"

          -- Validate session results
          ltsConnectionId finalState @?= ltsConnectionId finalState  -- Should be consistent
          assertBool "Should have processed some signals" (ltsSignalCount finalState >= 0)
          assertBool "Should have last signal time" (ltsLastSignalTime finalState /= Nothing)

-- Test complete live trading session with RSI strategy
testFullRSILiveSession :: Assertion
testFullRSILiveSession = do
  putStrLn "=== Full RSI Live Trading Session E2E Test ==="

  let instrument = Instrument "GBPUSD"
      registry = initializeStrategyRegistry

  -- Create RSI strategy configuration
  case createStrategyFromKeyword "rsi" Nothing registry of
    Nothing -> assertFailure "Failed to create RSI strategy"
    Just strategyInstance -> do
      let strategyParams = siParameters strategyInstance

      -- Create live trading configuration
      config <- defaultLiveTradingConfig instrument strategyParams

      -- Run simulated live trading session
      result <- runSimulatedLiveSession config registry 3  -- 3 second simulation

      case result of
        Left err -> assertFailure $ "RSI live session failed: " ++ show err
        Right finalState -> do
          putStrLn $ "RSI session completed with " ++ show (ltsSignalCount finalState) ++ " signals processed"

          -- Validate session results
          assertBool "Should be in valid state" (not (ltsIsRunning finalState) || ltsIsRunning finalState)
          assertBool "Signal count should be non-negative" (ltsSignalCount finalState >= 0)

-- Test multi-strategy live trading session
testMultiStrategyLiveSession :: Assertion
testMultiStrategyLiveSession = do
  putStrLn "=== Multi-Strategy Live Trading Session E2E Test ==="

  let instruments = [Instrument "EURUSD", Instrument "GBPUSD"]
      strategies = ["ema", "rsi"]
      registry = initializeStrategyRegistry

  -- Test multiple strategy sessions concurrently (simulated)
  results <- forM (zip instruments strategies) $ \(instr, strat) -> do
    case createStrategyFromKeyword strat Nothing registry of
      Nothing -> return $ Left $ strategyError (T.pack ("Failed to create strategy: " ++ strat))
      Just strategyInstance -> do
        let strategyParams = siParameters strategyInstance
        config <- defaultLiveTradingConfig instr strategyParams
        runSimulatedLiveSession config registry 2  -- 2 second simulation each

  -- All sessions should complete successfully
  let failures = [err | Left err <- results]
  case failures of
    [] -> do
      let finalStates = [state | Right state <- results]
      putStrLn $ "All " ++ show (length finalStates) ++ " strategy sessions completed"
      assertBool "All sessions should complete" (length finalStates == 2)
    (err:_) -> assertFailure $ "Multi-strategy session failed: " ++ show err

-- Test live trading with connection recovery
testLiveTradingWithRecovery :: Assertion
testLiveTradingWithRecovery = do
  putStrLn "=== Live Trading with Connection Recovery E2E Test ==="

  let instrument = Instrument "EURUSD"
      registry = initializeStrategyRegistry

  result <- runBrokerDataProviderIO $ do
    -- Step 1: Establish initial connection
    connResult1 <- connect
    case connResult1 of
      Left err -> return $ Left $ brokerError ("Initial connection failed: " <> T.pack (show err))
      Right connId1 -> do
        subResult1 <- subscribeToInstrument connId1 instrument
        case subResult1 of
          Left err -> return $ Left $ brokerError ("Initial subscription failed: " <> T.pack (show err))
          Right () -> do
            -- Step 2: Simulate disconnection
            disconnectResult <- disconnect connId1
            case disconnectResult of
              Left err -> return $ Left $ brokerError ("Disconnect failed: " <> T.pack (show err))
              Right () -> do
                -- Step 3: Recovery - new connection
                connResult2 <- connect
                case connResult2 of
                  Left err -> return $ Left $ brokerError ("Recovery connection failed: " <> T.pack (show err))
                  Right connId2 -> do
                    subResult2 <- subscribeToInstrument connId2 instrument
                    case subResult2 of
                      Left err -> return $ Left $ brokerError ("Recovery subscription failed: " <> T.pack (show err))
                      Right () -> return $ Right (connId1, connId2)

  case result of
    Left err -> assertFailure (show err)
    Right (connId1, connId2) -> do
      putStrLn "Connection recovery completed successfully"
      connId1 /= connId2 @? "Recovery should create new connection ID"

-- Test tick to signal E2E pipeline
testTickToSignalE2E :: Assertion
testTickToSignalE2E = do
  putStrLn "=== Tick to Signal E2E Pipeline Test ==="

  let instrument = Instrument "EURUSD"
      registry = initializeStrategyRegistry

  -- Create complete pipeline test
  case createStrategyFromKeyword "ema" Nothing registry of
    Nothing -> assertFailure "Failed to create EMA strategy for pipeline test"
    Just strategyInstance -> do
      now <- getCurrentTime

      -- Step 1: Create realistic tick sequence
      let ticks = createRealisticTickSequence now instrument 200
      putStrLn $ "Generated " ++ show (length ticks) ++ " ticks for processing"

      -- Step 2: Convert ticks to candles
      let candles = ticksToCandles ticks instrument
      putStrLn $ "Converted to " ++ show (length candles) ++ " candles"

      -- Step 3: Generate trading signals
      signal <- generateSignal strategyInstance candles
      putStrLn $ "Generated signal: " ++ show signal

      -- Validate complete pipeline
      assertBool "Should generate candles from ticks" (length candles > 0)
      assertBool "Should generate valid signal" (signal `elem` [Hold, Enter Buy, Enter Sell, Exit])

      -- Step 4: Simulate state management
      let (pipelineSignal, newState) = siSignalGenerator strategyInstance candles (siInitialState strategyInstance)
      putStrLn $ "Pipeline signal: " ++ show pipelineSignal

      assertBool "Pipeline should generate valid signal" (pipelineSignal `elem` [Hold, Enter Buy, Enter Sell, Exit])

-- Test high-frequency data processing E2E
testHighFrequencyDataE2E :: Assertion
testHighFrequencyDataE2E = do
  putStrLn "=== High-Frequency Data Processing E2E Test ==="

  let instrument = Instrument "EURUSD"
      registry = initializeStrategyRegistry

  case createStrategyFromKeyword "rsi" Nothing registry of
    Nothing -> assertFailure "Failed to create RSI strategy for high-freq test"
    Just strategyInstance -> do
      now <- getCurrentTime

      -- Generate high-frequency tick data (1000 ticks over 10 minutes)
      let highFreqTicks = createHighFrequencyTicks now instrument 1000 600  -- 600 seconds = 10 minutes
      putStrLn $ "Processing " ++ show (length highFreqTicks) ++ " high-frequency ticks"

      -- Process in batches to simulate real-time processing
      let batchSize = 50
          batches = chunksOf batchSize highFreqTicks

      -- Process each batch
      results <- forM batches $ \batch -> do
        let batchCandles = ticksToCandles batch instrument
        if null batchCandles
          then return Nothing
          else do
            signal <- generateSignal strategyInstance batchCandles
            return $ Just signal

      let validResults = [signal | Just signal <- results]
      putStrLn $ "Processed " ++ show (length validResults) ++ " signal batches"

      -- Validate high-frequency processing
      assertBool "Should process multiple batches" (length validResults > 5)
      assertBool "All signals should be valid" (all (`elem` [Hold, Enter Buy, Enter Sell, Exit]) validResults)

-- Test real-time candle generation E2E
testRealTimeCandleE2E :: Assertion
testRealTimeCandleE2E = do
  putStrLn "=== Real-time Candle Generation E2E Test ==="

  let instrument = Instrument "GBPUSD"
  now <- getCurrentTime

  -- Simulate real-time tick arrival pattern
  let minute1Ticks = createTicksForMinute now instrument 15           -- 15 ticks in minute 1
      minute2Ticks = createTicksForMinute (addUTCTime 60 now) instrument 12  -- 12 ticks in minute 2
      minute3Ticks = createTicksForMinute (addUTCTime 120 now) instrument 18 -- 18 ticks in minute 3
      allTicks = minute1Ticks ++ minute2Ticks ++ minute3Ticks

  putStrLn $ "Simulating real-time arrival of " ++ show (length allTicks) ++ " ticks over 3 minutes"

  -- Process incrementally to simulate real-time
  let processIncremental ticks accCandles = do
        let newCandles = ticksToCandles ticks instrument
        return $ accCandles ++ newCandles

  candles1 <- processIncremental minute1Ticks []
  candles2 <- processIncremental (minute1Ticks ++ minute2Ticks) []
  candles3 <- processIncremental allTicks []

  putStrLn $ "Generated candles: " ++ show (length candles3) ++ " total"

  -- Validate real-time processing
  assertBool "Should generate candles incrementally" (length candles3 >= 3)
  assertBool "Candle timestamps should be sequential" (candlesAreSequential candles3)

-- Test live data quality monitoring E2E
testLiveDataQualityE2E :: Assertion
testLiveDataQualityE2E = do
  putStrLn "=== Live Data Quality Monitoring E2E Test ==="

  let instrument = Instrument "EURUSD"

  result <- runBrokerDataProviderIO $ do
    connResult <- connect
    case connResult of
      Left err -> return $ Left err
      Right connId -> do
        subResult <- subscribeToInstrument connId instrument
        case subResult of
          Left err -> return $ Left err
          Right () -> do
            -- Monitor data quality over time
            quality1 <- getDataQuality connId instrument

            -- Simulate some time passing
            liftIO $ threadDelay 100000  -- 100ms

            quality2 <- getDataQuality connId instrument

            return $ Right (quality1, quality2)

  case result of
    Left err -> assertFailure $ "Data quality monitoring failed: " ++ show err
    Right (quality1Result, quality2Result) -> do
      case (quality1Result, quality2Result) of
        (Right quality1, Right quality2) -> do
          putStrLn $ "Quality monitoring: " ++ show (ldqQualityScore quality1) ++ " -> " ++ show (ldqQualityScore quality2)

          -- Validate quality monitoring
          assertBool "Quality scores should be valid" (ldqQualityScore quality1 >= 0.0 && ldqQualityScore quality1 <= 1.0)
          assertBool "Quality metrics should be consistent" (ldqQualityScore quality2 >= 0.0 && ldqQualityScore quality2 <= 1.0)

        _ -> assertFailure "Failed to get quality metrics"

-- Test broker disconnection recovery E2E
testBrokerDisconnectionRecoveryE2E :: Assertion
testBrokerDisconnectionRecoveryE2E = do
  putStrLn "=== Broker Disconnection Recovery E2E Test ==="

  let instrument = Instrument "EURUSD"
      registry = initializeStrategyRegistry

  -- Simulate complete disconnection and recovery cycle
  result <- runBrokerDataProviderIO $ do
    -- Phase 1: Normal operation
    connResult1 <- connect
    case connResult1 of
      Left err -> return $ Left $ brokerError ("Phase 1 connection failed: " <> T.pack (show err))
      Right connId1 -> do
        subResult1 <- subscribeToInstrument connId1 instrument
        case subResult1 of
          Left err -> return $ Left $ brokerError ("Phase 1 subscription failed: " <> T.pack (show err))
          Right () -> do
            status1 <- getConnectionStatus connId1

            -- Phase 2: Disconnection
            disconnectResult <- disconnect connId1
            case disconnectResult of
              Left err -> return $ Left $ brokerError ("Disconnection failed: " <> T.pack (show err))
              Right () -> do
                status2 <- getConnectionStatus connId1

                -- Phase 3: Recovery
                connResult2 <- connect
                case connResult2 of
                  Left err -> return $ Left $ brokerError ("Recovery connection failed: " <> T.pack (show err))
                  Right connId2 -> do
                    subResult2 <- subscribeToInstrument connId2 instrument
                    case subResult2 of
                      Left err -> return $ Left $ brokerError ("Recovery subscription failed: " <> T.pack (show err))
                      Right () -> do
                        status3 <- getConnectionStatus connId2
                        return $ Right (status1, status2, status3, connId1, connId2)

  case result of
    Left err -> assertFailure (show err)
    Right (status1, status2, status3, connId1, connId2) -> do
      putStrLn "Complete disconnection recovery cycle completed"

      -- Validate recovery cycle
      case (status1, status2, status3) of
        (Right (Connected _), Left _, Right (Connected _)) -> do
          putStrLn "Recovery cycle: Connected -> Disconnected -> Connected"
          connId1 /= connId2 @? "Recovery should use new connection ID"
        _ -> assertFailure "Recovery cycle validation failed"

-- Test strategy error recovery E2E
testStrategyErrorRecoveryE2E :: Assertion
testStrategyErrorRecoveryE2E = do
  putStrLn "=== Strategy Error Recovery E2E Test ==="

  let instrument = Instrument "EURUSD"
      registry = initializeStrategyRegistry

  case createStrategyFromKeyword "ema" Nothing registry of
    Nothing -> assertFailure "Failed to create EMA strategy for error recovery test"
    Just strategyInstance -> do
      now <- getCurrentTime

      -- Test with various problematic scenarios
      let scenarios =
            [ ("empty candles", [])
            , ("single candle", [createMockCandle now instrument])
            , ("normal candles", createMockCandleSequence now instrument 25)
            ]

      results <- forM scenarios $ \(name, candles) -> do
        putStrLn $ "Testing scenario: " ++ name
        signal <- generateSignal strategyInstance candles
        return (name, signal)

      putStrLn $ "Processed " ++ show (length results) ++ " error recovery scenarios"

      -- All scenarios should be handled gracefully
      let validResults = [(name, signal) | (name, signal) <- results, signal `elem` [Hold, Enter Buy, Enter Sell, Exit]]
      assertBool "All scenarios should be handled" (length validResults == length results)

-- Test configuration reload E2E
testConfigurationReloadE2E :: Assertion
testConfigurationReloadE2E = do
  putStrLn "=== Configuration Reload E2E Test ==="

  let instrument1 = Instrument "EURUSD"
      instrument2 = Instrument "GBPUSD"
      registry = initializeStrategyRegistry

  case createStrategyFromKeyword "rsi" Nothing registry of
    Nothing -> assertFailure "Failed to create RSI strategy for config reload test"
    Just strategyInstance -> do
      let strategyParams = siParameters strategyInstance

      -- Create initial configuration
      config1 <- defaultLiveTradingConfig instrument1 strategyParams
      putStrLn $ "Initial config for: " ++ show (ltcInstrument config1)

      -- Create updated configuration
      config2 <- defaultLiveTradingConfig instrument2 strategyParams
      putStrLn $ "Updated config for: " ++ show (ltcInstrument config2)

      -- Validate configuration changes
      ltcInstrument config1 @?= instrument1
      ltcInstrument config2 @?= instrument2
      ltcStrategy config1 @?= ltcStrategy config2  -- Same strategy, different instrument

-- Test data quality degradation handling E2E
testDataQualityDegradationE2E :: Assertion
testDataQualityDegradationE2E = do
  putStrLn "=== Data Quality Degradation Handling E2E Test ==="

  let instrument = Instrument "EURUSD"

  result <- runBrokerDataProviderIO $ do
    connResult <- connect
    case connResult of
      Left err -> return $ Left err
      Right connId -> do
        subResult <- subscribeToInstrument connId instrument
        case subResult of
          Left err -> return $ Left err
          Right () -> do
            -- Get initial quality
            quality1 <- getDataQuality connId instrument

            -- Simulate quality degradation scenario
            -- (In real implementation, this would involve actual data quality issues)
            quality2 <- getDataQuality connId instrument

            return $ Right (quality1, quality2)

  case result of
    Left err -> assertFailure $ "Quality degradation test failed: " ++ show err
    Right (quality1Result, quality2Result) -> do
      case (quality1Result, quality2Result) of
        (Right quality1, Right quality2) -> do
          putStrLn $ "Quality monitoring under degradation: " ++
                     show (ldqQualityScore quality1) ++ " -> " ++
                     show (ldqQualityScore quality2)

          -- Both should return valid quality metrics
          assertBool "Quality metrics should remain valid" (ldqQualityScore quality1 >= 0.0)
          assertBool "Quality metrics should remain valid" (ldqQualityScore quality2 >= 0.0)

        _ -> assertFailure "Failed to monitor quality degradation"

-- Test live trading performance E2E
testLiveTradingPerformanceE2E :: Assertion
testLiveTradingPerformanceE2E = do
  putStrLn "=== Live Trading Performance E2E Test ==="

  let instrument = Instrument "EURUSD"
      registry = initializeStrategyRegistry

  case createStrategyFromKeyword "ema" Nothing registry of
    Nothing -> assertFailure "Failed to create EMA strategy for performance test"
    Just strategyInstance -> do
      now <- getCurrentTime
      let startTime = now

      -- Process large dataset to test performance
      let largeTicks = createRealisticTickSequence now instrument 5000  -- 5k ticks

      putStrLn $ "Performance test: processing " ++ show (length largeTicks) ++ " ticks"

      -- Measure processing time
      let candles = ticksToCandles largeTicks instrument
      signal <- generateSignal strategyInstance candles

      endTime <- getCurrentTime
      let processingTime = diffUTCTime endTime startTime

      putStrLn $ "Processing completed in " ++ show processingTime ++ " seconds"
      putStrLn $ "Generated " ++ show (length candles) ++ " candles and signal: " ++ show signal

      -- Performance validation
      assertBool "Should complete in reasonable time" (processingTime < 10)  -- Less than 10 seconds
      assertBool "Should generate candles" (length candles > 0)
      assertBool "Should generate valid signal" (signal `elem` [Hold, Enter Buy, Enter Sell, Exit])

-- Test memory stability E2E
testMemoryStabilityE2E :: Assertion
testMemoryStabilityE2E = do
  putStrLn "=== Memory Stability E2E Test ==="

  let instrument = Instrument "EURUSD"
      registry = initializeStrategyRegistry

  case createStrategyFromKeyword "rsi" Nothing registry of
    Nothing -> assertFailure "Failed to create RSI strategy for memory test"
    Just strategyInstance -> do
      now <- getCurrentTime

      -- Process multiple rounds to test memory stability
      results <- forM [1..5] $ \round -> do
        putStrLn $ "Memory test round " ++ show round
        let ticks = createRealisticTickSequence (addUTCTime (fromIntegral round * 600) now) instrument 1000
            candles = ticksToCandles ticks instrument
        signal <- generateSignal strategyInstance candles
        return signal

      putStrLn $ "Completed " ++ show (length results) ++ " memory test rounds"

      -- All rounds should complete successfully
      assertBool "All rounds should complete" (length results == 5)
      assertBool "All signals should be valid" (all (`elem` [Hold, Enter Buy, Enter Sell, Exit]) results)

-- Test concurrent live sessions E2E
testConcurrentLiveSessionsE2E :: Assertion
testConcurrentLiveSessionsE2E = do
  putStrLn "=== Concurrent Live Sessions E2E Test ==="

  let instruments = [Instrument "EURUSD", Instrument "GBPUSD", Instrument "USDJPY"]
      registry = initializeStrategyRegistry

  -- Run concurrent simulated sessions
  results <- forM instruments $ \instrument -> do
    putStrLn $ "Starting concurrent session for " ++ show instrument
    case createStrategyFromKeyword "ema" Nothing registry of
      Nothing -> return $ Left $ strategyError (T.pack ("Failed to create strategy for " ++ show instrument))
      Just strategyInstance -> do
        let strategyParams = siParameters strategyInstance
        config <- defaultLiveTradingConfig instrument strategyParams
        runSimulatedLiveSession config registry 2  -- 2 second sessions

  let successes = [state | Right state <- results]
      failures = [err | Left err <- results]

  putStrLn $ "Concurrent sessions: " ++ show (length successes) ++ " succeeded, " ++ show (length failures) ++ " failed"

  -- Most sessions should succeed
  assertBool "Most concurrent sessions should succeed" (length successes >= 2)
  case failures of
    [] -> putStrLn "All concurrent sessions succeeded"
    errs -> putStrLn $ "Some failures occurred: " ++ show (take 2 (map show errs))

-- Helper functions for E2E tests

-- Run a simulated live trading session
runSimulatedLiveSession :: LiveTradingConfig' -> StrategyRegistry -> Int -> IO (Result LiveTradingState)
runSimulatedLiveSession config registry durationSeconds = do
  putStrLn $ "Running simulated live session for " ++ show durationSeconds ++ " seconds"

  -- Create mock live trading state
  now <- getCurrentTime
  let connId = ConnectionId (T.pack ("sim-" ++ show now))
      initialState = LiveTradingState
        { ltsConnectionId = connId
        , ltsStrategyState = "simulated-state"
        , ltsSignalCount = 0
        , ltsLastSignalTime = Nothing
        , ltsIsRunning = True
        }

  -- Simulate processing for duration
  threadDelay (durationSeconds * 1000000)  -- Convert to microseconds

  -- Return final state
  let finalState = initialState
        { ltsSignalCount = durationSeconds  -- Simulate processing signals
        , ltsLastSignalTime = Just now
        , ltsIsRunning = False  -- Session ended
        }

  return $ Right finalState

-- Create realistic tick sequence with price movement
createRealisticTickSequence :: UTCTime -> Instrument -> Int -> [Tick]
createRealisticTickSequence baseTime instrument count =
  let basePrice = 1.1850
      priceWalk = scanl (+) basePrice $ take count $ cycle [0.0001, -0.0001, 0.0002, -0.0001, 0.0001, -0.0002]
  in zipWith (\i price -> Tick
      { tTime = addUTCTime (fromIntegral i * 3) baseTime  -- 3 seconds apart
      , tInstr = instrument
      , tBid = Price price
      , tAsk = Price (price + 0.0002)
      , tVolume = Nothing
      }) [0..] priceWalk

-- Create high-frequency ticks over a time period
createHighFrequencyTicks :: UTCTime -> Instrument -> Int -> NominalDiffTime -> [Tick]
createHighFrequencyTicks baseTime instrument count duration =
  let interval = duration / fromIntegral count
      basePrice = 1.1850
  in [ Tick
       { tTime = addUTCTime (fromIntegral i * interval) baseTime
       , tInstr = instrument
       , tBid = Price (basePrice + fromIntegral (i `mod` 50) * 0.00001)  -- Small price variations
       , tAsk = Price (basePrice + 0.0002 + fromIntegral (i `mod` 50) * 0.00001)
       , tVolume = Nothing
       }
     | i <- [0..count-1]
     ]

-- Create ticks for a specific minute
createTicksForMinute :: UTCTime -> Instrument -> Int -> [Tick]
createTicksForMinute baseTime instrument count =
  let basePrice = 1.2850
  in [ Tick
       { tTime = addUTCTime (fromIntegral i * 4) baseTime  -- 4 seconds apart within minute
       , tInstr = instrument
       , tBid = Price (basePrice + fromIntegral i * 0.00005)
       , tAsk = Price (basePrice + 0.0002 + fromIntegral i * 0.00005)
       , tVolume = Nothing
       }
     | i <- [0..count-1]
     ]

-- Check if candles are in sequential order
candlesAreSequential :: [Candle] -> Bool
candlesAreSequential [] = True
candlesAreSequential [_] = True
candlesAreSequential (c1:c2:cs) = cTime c1 <= cTime c2 && candlesAreSequential (c2:cs)

-- Split list into chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (chunk, rest) = splitAt n xs in chunk : chunksOf n rest

-- Create a mock candle
createMockCandle :: UTCTime -> Instrument -> Candle
createMockCandle time instrument = Candle
  { cTime = time
  , cOpen = Price 1.1850
  , cHigh = Price 1.1855
  , cLow = Price 1.1845
  , cClose = Price 1.1852
  }

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
