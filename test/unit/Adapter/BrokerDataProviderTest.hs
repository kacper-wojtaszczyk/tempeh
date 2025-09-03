{-# LANGUAGE OverloadedStrings #-}
module Unit.Adapter.BrokerDataProviderTest where

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Time
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Scientific
import Data.List (group, sort)

import Adapter.BrokerDataProvider
import Domain.Services.LiveDataService
import Domain.Types
import Util.Config
import Util.Error

-- Test suite for BrokerDataProvider
tests :: TestTree
tests = brokerDataProviderTests

brokerDataProviderTests :: TestTree
brokerDataProviderTests = testGroup "Adapter.BrokerDataProvider"
  [ testGroup "Connection Management"
    [ testCase "Demo connection should succeed" testDemoConnection
    , testCase "Connection status should be trackable" testConnectionStatus
    , testCase "Multiple connections should be managed" testMultipleConnections
    ]
  , testGroup "Subscription Management"
    [ testCase "Instrument subscription should work" testInstrumentSubscription
    , testCase "Multiple instrument subscriptions" testMultipleSubscriptions
    , testCase "Unsubscribe should remove subscription" testUnsubscribe
    ]
  , testGroup "Tick Stream Management"
    [ testCase "Tick stream should be accessible after subscription" testTickStream
    , testCase "Tick stream should be empty initially" testEmptyTickStream
    , testCase "Unsubscribed instrument should fail tick stream access" testUnsubscribedTickStream
    ]
  , testGroup "Data Quality Monitoring"
    [ testCase "Data quality should return valid metrics" testDataQuality
    , testCase "Data quality metrics should be realistic" testDataQualityMetrics
    ]
  , testGroup "IG API Integration"
    [ testCase "IG epic mapping should work for common pairs" testIGEpicMapping
    , testCase "IG market data conversion should preserve data" testIGMarketDataConversion
    , testCase "Unknown instruments should be handled" testUnknownInstruments
    ]
  , testGroup "Error Handling"
    [ testCase "Invalid connection ID should return error" testInvalidConnectionId
    , testCase "Connection failures should be handled gracefully" testConnectionFailures
    ]
  , testGroup "Property-based Tests"
    [ QC.testProperty "Connection IDs should be unique" prop_connectionIdsUnique
    , QC.testProperty "Tick data should preserve instrument info" prop_tickDataPreservation
    ]
  ]

-- Demo connection test
testDemoConnection :: Assertion
testDemoConnection = do
  result <- runBrokerDataProviderIO $ do
    connResult <- connect
    case connResult of
      Left err -> return $ Left err
      Right connId -> do
        status <- getConnectionStatus connId
        return $ Right (connId, status)

  case result of
    Left err -> assertFailure $ "Demo connection failed: " ++ show err
    Right (connId, statusResult) -> do
      case statusResult of
        Left err -> assertFailure $ "Failed to get connection status: " ++ show err
        Right status -> case status of
          Connected _ -> return () -- Success
          _ -> assertFailure $ "Expected Connected status, got: " ++ show status

-- Connection status tracking test
testConnectionStatus :: Assertion
testConnectionStatus = do
  result <- runBrokerDataProviderIO $ do
    connResult <- connect
    case connResult of
      Left err -> return $ Left err
      Right connId -> do
        status1 <- getConnectionStatus connId
        -- Disconnect and check status
        disconnectResult <- disconnect connId
        status2 <- getConnectionStatus connId
        return $ Right (status1, disconnectResult, status2)

  case result of
    Left err -> assertFailure $ "Connection test failed: " ++ show err
    Right (status1, disconnectResult, status2) -> do
      case (status1, disconnectResult, status2) of
        (Right (Connected _), Right (), Left _) -> return () -- Expected: connected, then not found
        _ -> assertFailure "Connection status tracking failed"

-- Multiple connections test
testMultipleConnections :: Assertion
testMultipleConnections = do
  result <- runBrokerDataProviderIO $ do
    conn1Result <- connect
    conn2Result <- connect
    case (conn1Result, conn2Result) of
      (Right connId1, Right connId2) -> do
        status1 <- getConnectionStatus connId1
        status2 <- getConnectionStatus connId2
        return $ Right (connId1, connId2, status1, status2)
      _ -> return $ Left "Failed to create multiple connections"

  case result of
    Left err -> assertFailure err
    Right (connId1, connId2, status1, status2) -> do
      -- Connection IDs should be different
      assertBool "Connection IDs should differ" (connId1 /= connId2)
      -- Both should be connected
      case (status1, status2) of
        (Right (Connected _), Right (Connected _)) -> return ()
        _ -> assertFailure "Multiple connections failed"

-- Instrument subscription test
testInstrumentSubscription :: Assertion
testInstrumentSubscription = do
  let instrument = Instrument "EURUSD"
  result <- runBrokerDataProviderIO $ do
    connResult <- connect
    case connResult of
      Left err -> return $ Left err
      Right connId -> do
        subResult <- subscribeToInstrument connId instrument
        return $ Right (connId, subResult)

  case result of
    Left err -> assertFailure $ "Subscription test setup failed: " ++ show err
    Right (connId, subResult) -> case subResult of
      Right () -> return () -- Success
      Left err -> assertFailure $ "Subscription failed: " ++ show err

-- Multiple subscriptions test
testMultipleSubscriptions :: Assertion
testMultipleSubscriptions = do
  let instruments = [Instrument "EURUSD", Instrument "GBPUSD", Instrument "USDJPY"]
  result <- runBrokerDataProviderIO $ do
    connResult <- connect
    case connResult of
      Left err -> return $ Left err
      Right connId -> do
        subResults <- mapM (subscribeToInstrument connId) instruments
        return $ Right (connId, subResults)

  case result of
    Left err -> assertFailure $ "Multiple subscription test setup failed: " ++ show err
    Right (connId, subResults) -> do
      -- All subscriptions should succeed
      mapM_ (\subResult -> case subResult of
        Right () -> return ()
        Left err -> assertFailure $ "Subscription failed: " ++ show err
        ) subResults

-- Unsubscribe test
testUnsubscribe :: Assertion
testUnsubscribe = do
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
            unsubResult <- unsubscribeFromInstrument connId instrument
            -- Try to get tick stream after unsubscribe (should fail)
            tickStreamResult <- getTickStream connId instrument
            return $ Right (unsubResult, tickStreamResult)

  case result of
    Left err -> assertFailure $ "Unsubscribe test setup failed: " ++ show err
    Right (unsubResult, tickStreamResult) -> do
      case unsubResult of
        Right () -> return () -- Unsubscribe succeeded
        Left err -> assertFailure $ "Unsubscribe failed: " ++ show err
      -- Tick stream should fail after unsubscribe
      case tickStreamResult of
        Left _ -> return () -- Expected failure
        Right _ -> assertFailure "Tick stream should fail after unsubscribe"

-- Tick stream access test
testTickStream :: Assertion
testTickStream = do
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
            tickStreamResult <- getTickStream connId instrument
            return $ Right tickStreamResult

  case result of
    Left err -> assertFailure $ "Tick stream test setup failed: " ++ show err
    Right tickStreamResult -> case tickStreamResult of
      Right _ -> return () -- Success - we can access the tick stream
      Left err -> assertFailure $ "Failed to get tick stream: " ++ show err

-- Empty tick stream test
testEmptyTickStream :: Assertion
testEmptyTickStream = do
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
            tickStreamResult <- getTickStream connId instrument
            case tickStreamResult of
              Left err -> return $ Left err
              Right tickStream -> do
                ticks <- liftIO $ atomically tickStream
                return $ Right ticks

  case result of
    Left err -> assertFailure $ "Empty tick stream test setup failed: " ++ show err
    Right ticks -> do
      -- Initially, tick stream should be empty
      length ticks @?= 0

-- Unsubscribed tick stream test
testUnsubscribedTickStream :: Assertion
testUnsubscribedTickStream = do
  let instrument = Instrument "EURUSD"
  result <- runBrokerDataProviderIO $ do
    connResult <- connect
    case connResult of
      Left err -> return $ Left err
      Right connId -> do
        -- Try to get tick stream without subscribing
        tickStreamResult <- getTickStream connId instrument
        return $ Right tickStreamResult

  case result of
    Left err -> assertFailure $ "Unsubscribed tick stream test setup failed: " ++ show err
    Right tickStreamResult -> case tickStreamResult of
      Left _ -> return () -- Expected failure
      Right _ -> assertFailure "Tick stream should fail for unsubscribed instrument"

-- Data quality test
testDataQuality :: Assertion
testDataQuality = do
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
            qualityResult <- getDataQuality connId instrument
            return $ Right qualityResult

  case result of
    Left err -> assertFailure $ "Data quality test setup failed: " ++ show err
    Right qualityResult -> case qualityResult of
      Right quality -> do
        -- Validate quality metrics
        ldqQualityScore quality @?= 0.98
        ldqTicksReceived quality @?= 100
        ldqTicksExpected quality @?= 100
      Left err -> assertFailure $ "Failed to get data quality: " ++ show err

-- Data quality metrics test
testDataQualityMetrics :: Assertion
testDataQualityMetrics = do
  let instrument = Instrument "GBPUSD"
  result <- runBrokerDataProviderIO $ do
    connResult <- connect
    case connResult of
      Left err -> return $ Left err
      Right connId -> do
        subResult <- subscribeToInstrument connId instrument
        case subResult of
          Left err -> return $ Left err
          Right () -> do
            qualityResult <- getDataQuality connId instrument
            return $ Right qualityResult

  case result of
    Left err -> assertFailure $ "Data quality metrics test setup failed: " ++ show err
    Right qualityResult -> case qualityResult of
      Right quality -> do
        -- Quality score should be between 0 and 1
        let score = ldqQualityScore quality
        assertBool "Quality score should be between 0 and 1" (score >= 0.0 && score <= 1.0)
        -- Ticks received should be non-negative
        assertBool "Ticks received should be non-negative" (ldqTicksReceived quality >= 0)
        -- Latency should be positive if present
        case ldqLatency quality of
          Just latency -> assertBool "Latency should be positive" (latency > 0)
          Nothing -> return ()
      Left err -> assertFailure $ "Failed to get data quality metrics: " ++ show err

-- IG epic mapping test
testIGEpicMapping :: Assertion
testIGEpicMapping = do
  -- Test common forex pairs
  instrumentToIGEpic (Instrument "EURUSD") @?= Just "CS.D.EURUSD.CFD.IP"
  instrumentToIGEpic (Instrument "GBPUSD") @?= Just "CS.D.GBPUSD.CFD.IP"
  instrumentToIGEpic (Instrument "USDJPY") @?= Just "CS.D.USDJPY.CFD.IP"
  instrumentToIGEpic (Instrument "AUDUSD") @?= Just "CS.D.AUDUSD.CFD.IP"
  instrumentToIGEpic (Instrument "USDCAD") @?= Just "CS.D.USDCAD.CFD.IP"

-- IG market data conversion test
testIGMarketDataConversion :: Assertion
testIGMarketDataConversion = do
  let igMarket = IGMarket
        { marketEpic = "CS.D.EURUSD.CFD.IP"
        , marketInstrument = "EUR/USD"
        , marketBid = Just 1.1850
        , marketAsk = Just 1.1852
        , marketUpdateTime = Just "2025-09-03T17:30:00"
        }
      instrument = Instrument "EURUSD"

  now <- getCurrentTime
  let tick = convertIGMarketToTick igMarket instrument now

  -- Validate conversion
  tInstr tick @?= instrument
  tTime tick @?= now
  unPrice (tBid tick) @?= 1.1850
  unPrice (tAsk tick) @?= 1.1852
  tVolume tick @?= Nothing

-- Unknown instruments test
testUnknownInstruments :: Assertion
testUnknownInstruments = do
  -- Unknown instrument should return Nothing for epic mapping
  instrumentToIGEpic (Instrument "UNKNOWN") @?= Nothing
  instrumentToIGEpic (Instrument "FAKE") @?= Nothing
  instrumentToIGEpic (Instrument "") @?= Nothing

-- Invalid connection ID test
testInvalidConnectionId :: Assertion
testInvalidConnectionId = do
  let invalidConnId = ConnectionId "invalid-connection-id"
      instrument = Instrument "EURUSD"

  result <- runBrokerDataProviderIO $ do
    statusResult <- getConnectionStatus invalidConnId
    subResult <- subscribeToInstrument invalidConnId instrument
    return (statusResult, subResult)

  case result of
    (Left _, Left _) -> return () -- Both should fail as expected
    _ -> assertFailure "Invalid connection ID should return errors"

-- Connection failures test
testConnectionFailures :: Assertion
testConnectionFailures = do
  result <- runBrokerDataProviderIO $ do
    -- Test disconnect of non-existent connection
    let fakeConnId = ConnectionId "fake-connection"
    disconnectResult <- disconnect fakeConnId
    return disconnectResult

  case result of
    Right () -> return () -- Disconnect should succeed (idempotent)
    Left err -> assertFailure $ "Disconnect should be idempotent: " ++ show err

-- Property: Connection IDs should be unique
prop_connectionIdsUnique :: Property
prop_connectionIdsUnique = monadicIO $ do
  connIds <- run $ runBrokerDataProviderIO $ do
    results <- sequence $ replicate 10 connect
    return $ [connId | Right connId <- results]

  -- All connection IDs should be unique
  assert $ length connIds == length (nubOrd connIds)
  where
    nubOrd :: Ord a => [a] -> [a]
    nubOrd = map head . group . sort

-- Property: Tick data should preserve instrument info
prop_tickDataPreservation :: Instrument -> Property
prop_tickDataPreservation instrument = monadicIO $ do
  result <- run $ runBrokerDataProviderIO $ do
    connResult <- connect
    case connResult of
      Left err -> return $ Left err
      Right connId -> do
        subResult <- subscribeToInstrument connId instrument
        case subResult of
          Left err -> return $ Left err
          Right () -> do
            tickStreamResult <- getTickStream connId instrument
            return $ Right (connId, tickStreamResult)

  case result of
    Left _ -> return () -- Skip if setup failed
    Right (connId, tickStreamResult) -> case tickStreamResult of
      Left _ -> return () -- Skip if tick stream failed
      Right tickStream -> do
        ticks <- run $ atomically tickStream
        -- All ticks should have the correct instrument
        assert $ all (\tick -> tInstr tick == instrument) ticks

-- Generate arbitrary instruments for property testing
instance Arbitrary Instrument where
  arbitrary = do
    base <- elements ["EUR", "GBP", "USD", "JPY", "AUD", "CAD", "CHF", "NZD"]
    quote <- elements ["EUR", "GBP", "USD", "JPY", "AUD", "CAD", "CHF", "NZD"]
    return $ Instrument (T.pack (base ++ quote))
