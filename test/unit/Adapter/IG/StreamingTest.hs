{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}

module Unit.Adapter.IG.StreamingTest where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Control.Exception (try, SomeException)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)

import Adapter.IG.Streaming
import Adapter.IG.Types
import Domain.Types
import Util.Config
import Util.Error

-- Test suite
tests :: TestTree
tests = testGroup "Adapter.IG.Streaming"
  [ testGroup "Protocol Parsing"
    [ testCase "Parse CONOK response" testParseConokResponse
    , testCase "Parse error response" testParseErrorResponse
    , testCase "Parse ping message" testParsePingMessage
    , testCase "Parse loop message" testParseLoopMessage
    , testCase "Parse update message" testParseUpdateMessage
    , testCase "Parse invalid message" testParseInvalidMessage
    ]
  , testGroup "Control Message Formatting"
    [ testCase "Format create session message" testFormatCreateSession
    , testCase "Format bind session message" testFormatBindSession
    , testCase "Format subscribe message" testFormatSubscribe
    , testCase "Format unsubscribe message" testFormatUnsubscribe
    ]
  , testGroup "Instrument Mapping"
    [ testCase "IG epic mapping for major pairs" testIGEpicMapping
    , testCase "IG epic fallback for unknown instruments" testIGEpicFallback
    ]
  , testGroup "Tick Processing"
    [ testCase "Handle streaming tick with valid data" testHandleValidTick
    , testCase "Handle streaming tick with missing data" testHandleMissingData
    , testCase "Handle streaming tick for unknown subscription" testHandleUnknownSubscription
    ]
  , testGroup "Property-based Tests"
    [ QC.testProperty "All instruments have valid epics" prop_instrumentsHaveEpics
    , QC.testProperty "Control messages are properly formatted" prop_controlMessagesFormat
    ]
  ]

-- Test data
testInstrument :: Instrument
testInstrument = Instrument "EURUSD"

testBrokerConfig :: BrokerConfig
testBrokerConfig = BrokerConfig
  { bcBrokerType = IG
  , bcEnvironment = DemoEnv
  , bcBaseUrl = Just "https://demo-api.ig.com"
  , bcApiKey = Just "test-api-key"
  , bcUsername = Just "test-user"
  , bcPassword = Just "test-pass"
  , bcAccountId = Just "test-account"
  , bcConnectTimeout = 5
  , bcReadTimeout = 10
  , bcReconnectPolicy = ReconnectPolicy 3 1.0 10.0 2.0
  }

testIGSession :: IGSession
testIGSession = IGSession
  { igSessionToken = "test-token"
  , igCST = "test-cst"
  , igXSecurityToken = "test-security-token"
  , igExpiresAt = read "2025-12-31 23:59:59 UTC"
  , igLightstreamerEndpoint = Just "https://demo-apd.marketdatasystems.com"
  }

-- Protocol parsing tests
testParseConokResponse :: Assertion
testParseConokResponse = do
  let message = "CONOK,session_id=S1234567&timeout=5000"
      result = parseLightstreamerMessage message

  case result of
    LSControlResponse "CONOK" params -> do
      lookup "session_id" params @?= Just "S1234567"
      lookup "timeout" params @?= Just "5000"
    _ -> assertFailure $ "Expected CONOK response, got: " ++ show result

testParseErrorResponse :: Assertion
testParseErrorResponse = do
  let message = "ERROR Invalid credentials"
      result = parseLightstreamerMessage message

  case result of
    LSError errMsg -> errMsg @?= " Invalid credentials"
    _ -> assertFailure $ "Expected error response, got: " ++ show result

testParsePingMessage :: Assertion
testParsePingMessage = do
  let message = "PING"
      result = parseLightstreamerMessage message

  result @?= LSPing

testParseLoopMessage :: Assertion
testParseLoopMessage = do
  let message = "LOOP 5000"
      result = parseLightstreamerMessage message

  result @?= LSLoop

testParseUpdateMessage :: Assertion
testParseUpdateMessage = do
  let message = "1|1.1850|1.1852|1641024000000"
      result = parseLightstreamerMessage message

  case result of
    LSUpdateMessage subId values -> do
      subId @?= 1
      values @?= [Just "1.1850", Just "1.1852", Just "1641024000000"]
    _ -> assertFailure $ "Expected update message, got: " ++ show result

testParseInvalidMessage :: Assertion
testParseInvalidMessage = do
  let message = "UNKNOWN MESSAGE FORMAT"
      result = parseLightstreamerMessage message

  case result of
    LSError errMsg -> T.isInfixOf "Unknown message" errMsg @?= True
    _ -> assertFailure $ "Expected error for invalid message, got: " ++ show result

-- Control message formatting tests
testFormatCreateSession :: Assertion
testFormatCreateSession = do
  let msg = CreateSession "https://example.com" "QUOTE_ADAPTER"
      result = formatControlMessage msg
      expected = "create_session\r\nLS_adapter_set=QUOTE_ADAPTER&LS_cid=mgQkwtwdysogQz2BJ4Ji%20kOj2Bg&LS_send_sync=false&LS_cause=api\r\n"

  result @?= expected

testFormatBindSession :: Assertion
testFormatBindSession = do
  let msg = BindSession "S1234567" "C9876543"
      result = formatControlMessage msg
      expected = "bind_session\r\nLS_session=S1234567&LS_keepalive_millis=5000&LS_send_sync=false&LS_cause=ws.loop\r\n"

  result @?= expected

testFormatSubscribe :: Assertion
testFormatSubscribe = do
  let msg = Subscribe ["MARKET:CS.D.EURUSD.MINI.IP"] ["BID", "OFR", "UTM"] "MERGE"
      result = formatControlMessage msg
      expected = "control\r\nLS_reqId=1&LS_op=add&LS_mode=MERGE&LS_group=MARKET:CS.D.EURUSD.MINI.IP&LS_schema=BID OFR UTM\r\n"

  result @?= expected

testFormatUnsubscribe :: Assertion
testFormatUnsubscribe = do
  let msg = Unsubscribe 1
      result = formatControlMessage msg
      expected = "control\r\nLS_reqId=1&LS_op=delete\r\n"

  result @?= expected

-- Instrument mapping tests
testIGEpicMapping :: Assertion
testIGEpicMapping = do
  instrumentToIGEpic (Instrument "EURUSD") @?= Just "CS.D.EURUSD.MINI.IP"
  instrumentToIGEpic (Instrument "GBPUSD") @?= Just "CS.D.GBPUSD.MINI.IP"
  instrumentToIGEpic (Instrument "USDJPY") @?= Just "CS.D.USDJPY.MINI.IP"
  instrumentToIGEpic (Instrument "AUDUSD") @?= Just "CS.D.AUDUSD.MINI.IP"
  instrumentToIGEpic (Instrument "USDCAD") @?= Just "CS.D.USDCAD.MINI.IP"

testIGEpicFallback :: Assertion
testIGEpicFallback = do
  -- Unknown instruments should return Nothing
  instrumentToIGEpic (Instrument "UNKNOWN") @?= Nothing
  instrumentToIGEpic (Instrument "FAKE") @?= Nothing
  instrumentToIGEpic (Instrument "") @?= Nothing

-- Tick processing tests (using mock LSConnection)
createMockLSConnection :: IO LSConnection
createMockLSConnection = do
  subscriptions <- newTVarIO Map.empty
  nextSubId <- newTVarIO 1
  return LSConnection
    { lsConnection = undefined  -- We won't use the actual WebSocket connection in tests
    , lsSessionId = Just "TEST_SESSION"
    , lsControlUrl = "https://example.com"
    , lsSubscriptions = subscriptions
    , lsNextSubId = nextSubId
    , lsHeartbeatAsync = Nothing
    , lsMessageAsync = Nothing
    }

testHandleValidTick :: Assertion
testHandleValidTick = do
  conn <- createMockLSConnection
  tickBuffer <- newTVarIO []

  -- Create a subscription
  let subscription = LSSubscription
        { lsSubId = 1
        , lsItems = ["MARKET:CS.D.EURUSD.MINI.IP"]
        , lsFields = ["BID", "OFR", "UTM"]
        , lsInstrument = testInstrument
        , lsTickBuffer = tickBuffer
        }

  atomically $ modifyTVar (lsSubscriptions conn) $ Map.insert 1 subscription

  -- Handle a valid tick
  handleStreamingTick conn 1 [Just "1.1850", Just "1.1852", Just "1641024000000"]

  -- Check that tick was added to buffer
  ticks <- readTVarIO tickBuffer
  length ticks @?= 1

  let tick = head ticks
  tInstr tick @?= testInstrument
  unPrice (tBid tick) @?= 1.1850
  unPrice (tAsk tick) @?= 1.1852

testHandleMissingData :: Assertion
testHandleMissingData = do
  conn <- createMockLSConnection
  tickBuffer <- newTVarIO []

  -- Create a subscription
  let subscription = LSSubscription
        { lsSubId = 1
        , lsItems = ["MARKET:CS.D.EURUSD.MINI.IP"]
        , lsFields = ["BID", "OFR", "UTM"]
        , lsInstrument = testInstrument
        , lsTickBuffer = tickBuffer
        }

  atomically $ modifyTVar (lsSubscriptions conn) $ Map.insert 1 subscription

  -- Handle tick with missing bid price
  handleStreamingTick conn 1 [Nothing, Just "1.1852", Just "1641024000000"]

  -- Check that no tick was added due to missing data
  ticks <- readTVarIO tickBuffer
  length ticks @?= 0

testHandleUnknownSubscription :: Assertion
testHandleUnknownSubscription = do
  conn <- createMockLSConnection

  -- Try to handle tick for non-existent subscription
  -- This should not crash, just log a warning
  handleStreamingTick conn 999 [Just "1.1850", Just "1.1852", Just "1641024000000"]

  -- Test passes if no exception is thrown
  return ()

-- Property-based tests
prop_instrumentsHaveEpics :: Instrument -> Bool
prop_instrumentsHaveEpics instrument =
  case instrumentToIGEpic instrument of
    Nothing -> True  -- It's valid for unsupported instruments to return Nothing
    Just epic -> not (T.null epic) && T.isInfixOf "CS.D." epic && T.isInfixOf ".MINI.IP" epic

prop_controlMessagesFormat :: LSControlMessage -> Bool
prop_controlMessagesFormat msg =
  let formatted = formatControlMessage msg
  in case msg of
    CreateSession _ _ -> "create_session" `T.isInfixOf` formatted
    BindSession _ _ -> "bind_session" `T.isInfixOf` formatted
    Subscribe _ _ _ -> "control" `T.isInfixOf` formatted && "LS_op=add" `T.isInfixOf` formatted
    Unsubscribe _ -> "control" `T.isInfixOf` formatted && "LS_op=delete" `T.isInfixOf` formatted

-- Arbitrary instances for property-based testing
instance Arbitrary Instrument where
  arbitrary = do
    base <- elements ["EUR", "GBP", "USD", "JPY", "AUD", "CHF", "CAD"]
    quote <- elements ["USD", "EUR", "GBP", "JPY", "CHF", "CAD", "AUD"]
    return $ Instrument (T.pack (base ++ quote))

instance Arbitrary LSControlMessage where
  arbitrary = oneof
    [ CreateSession <$> arbitraryUrl <*> arbitrary
    , BindSession <$> arbitrarySessionId <*> arbitraryConnectionId
    , Subscribe <$> listOf1 arbitraryItem <*> listOf1 arbitraryField <*> arbitraryMode
    , Unsubscribe <$> arbitrary
    ]
    where
      arbitraryUrl = elements ["https://example.com", "wss://stream.example.com"]
      arbitrarySessionId = elements ["S123", "S456", "S789"]
      arbitraryConnectionId = elements ["C123", "C456", "C789"]
      arbitraryItem = elements ["MARKET:CS.D.EURUSD.MINI.IP", "MARKET:CS.D.GBPUSD.MINI.IP"]
      arbitraryField = elements ["BID", "OFR", "UTM", "VOL"]
      arbitraryMode = elements ["MERGE", "DISTINCT", "RAW"]

instance Arbitrary Text where
  arbitrary = T.pack <$> listOf1 (elements ['a'..'z'])
