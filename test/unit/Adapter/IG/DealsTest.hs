{-# LANGUAGE OverloadedStrings #-}

module Unit.Adapter.IG.DealsTest
  ( dealsTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime, addUTCTime)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as L8

import Adapter.IG.Types
import Adapter.IG.Deals
import Util.Config (BrokerConfig(..), defaultBrokerConfig)
import Util.Error (Result(..))

-- Test data setup
testConfig :: BrokerConfig
testConfig = defaultBrokerConfig
  { bcBaseUrl = Just "https://demo-api.ig.com/gateway/deal"
  , bcApiKey = Just "test-api-key"
  }

testSession :: IO IGSession
testSession = do
  now <- getCurrentTime
  return $ IGSession
    { igSessionToken = "test-session-token"
    , igCST = "test-cst"
    , igXSecurityToken = "test-x-security-token"
    , igExpiresAt = addUTCTime 3600 now
    , igLightstreamerEndpoint = Just "https://demo-api.ig.com/lightstreamer"
    }

-- Sample test data
samplePosition :: IGPosition
samplePosition = IGPosition
  { positionDealId = "DIAAAABBBCCC123"
  , positionDealReference = Just "REF-001"
  , positionDirection = BUY
  , positionSize = 1.0
  , positionLevel = 1.1005
  , positionStopLevel = Just 1.0950
  , positionLimitLevel = Just 1.1050
  , positionTrailingStop = False
  , positionCurrency = "USD"
  , positionCreatedDateUTC = "2025-09-07T12:00:00Z"
  , positionEpic = "CS.D.EURUSD.CFD.IP"
  , positionInstrumentName = "EUR/USD"
  , positionBid = Just 1.1003
  , positionOffer = Just 1.1005
  , positionMarketStatus = TRADEABLE
  }

sampleDealRequest :: IGDealRequest
sampleDealRequest = IGDealRequest
  { dealEpic = "CS.D.EURUSD.CFD.IP"
  , dealExpiry = "-"
  , dealDirection = BUY
  , dealSize = 1.0
  , dealOrderType = MARKET
  , dealLevel = Nothing
  , dealQuoteId = Nothing
  , dealCurrencyCode = Just "USD"
  , dealForceOpen = Just True
  , dealGuaranteedStop = Just False
  , dealStopLevel = Just 1.0950
  , dealStopDistance = Nothing
  , dealTrailingStop = Just False
  , dealTrailingStopIncrement = Nothing
  , dealLimitLevel = Just 1.1050
  , dealLimitDistance = Nothing
  , dealTimeInForce = Nothing
  , dealReference = Just "client-ref-001"
  }

sampleDealResponse :: IGDealResponse
sampleDealResponse = IGDealResponse
  { dealResponseReference = "D8VEMD26FGQTYPH"
  , dealResponseStatus = Just ACCEPTED
  }

sampleWorkingOrder :: IGWorkingOrder
sampleWorkingOrder = IGWorkingOrder
  { workingOrderDealId = "W1234"
  , workingOrderDirection = BUY
  , workingOrderOrderType = LIMIT
  , workingOrderLevel = 1.0750
  , workingOrderSize = 1.0
  , workingOrderTimeInForce = GOOD_TILL_CANCELLED
  , workingOrderGoodTillDate = Nothing
  , workingOrderCreatedDateUTC = "2025-09-01T10:00:00Z"
  , workingOrderEpic = "CS.D.EURUSD.CFD.IP"
  , workingOrderInstrumentName = "EUR/USD"
  , workingOrderBid = Just 1.1003
  , workingOrderOffer = Just 1.1005
  , workingOrderMarketStatus = TRADEABLE
  }

sampleWorkingOrderRequest :: IGWorkingOrderRequest
sampleWorkingOrderRequest = IGWorkingOrderRequest
  { workingOrderReqEpic = "CS.D.GBPUSD.CFD.IP"
  , workingOrderReqExpiry = "-"
  , workingOrderReqDirection = SELL
  , workingOrderReqSize = 2.0
  , workingOrderReqOrderType = LIMIT
  , workingOrderReqLevel = 1.2500
  , workingOrderReqLimitLevel = Just 1.2400
  , workingOrderReqLimitDistance = Nothing
  , workingOrderReqStopLevel = Just 1.2600
  , workingOrderReqStopDistance = Nothing
  , workingOrderReqGuaranteedStop = Just False
  , workingOrderReqTimeInForce = Just GOOD_TILL_DATE
  , workingOrderReqGoodTillDate = Just "2025-12-31T23:59:59Z"
  , workingOrderReqCurrencyCode = Just "USD"
  , workingOrderReqForceOpen = Just True
  , workingOrderReqDealReference = Just "working-client-001"
  }

sampleDealConfirmation :: IGDealConfirmation
sampleDealConfirmation = IGDealConfirmation
  { confirmationDealReference = "D8VEMD26FGQTYPH"
  , confirmationDealId = Just "DIAAAABBBCCC123"
  , confirmationEpic = Just "CS.D.EURUSD.CFD.IP"
  , confirmationDealStatus = Just ACCEPTED
  , confirmationDirection = Just BUY
  , confirmationSize = Just 1.0
  , confirmationLevel = Just 1.10050
  , confirmationTimeStamp = Just "2025-09-07T12:00:00Z"
  , confirmationReason = Nothing
  }

-- JSON serialization tests
jsonSerializationTests :: TestTree
jsonSerializationTests = testGroup "JSON Serialization Tests"
  [ testCase "Direction serialization" $ do
      encode BUY @?= "\"BUY\""
      encode SELL @?= "\"SELL\""
      decode "\"BUY\"" @?= Just BUY
      decode "\"SELL\"" @?= Just SELL

  , testCase "OrderType serialization" $ do
      encode MARKET @?= "\"MARKET\""
      encode LIMIT @?= "\"LIMIT\""
      encode STOP @?= "\"STOP\""
      decode "\"MARKET\"" @?= Just MARKET
      decode "\"LIMIT\"" @?= Just LIMIT

  , testCase "DealStatus serialization" $ do
      encode ACCEPTED @?= "\"ACCEPTED\""
      encode REJECTED @?= "\"REJECTED\""
      encode PENDING @?= "\"PENDING\""
      decode "\"ACCEPTED\"" @?= Just ACCEPTED
      decode "\"REJECTED\"" @?= Just REJECTED

  , testCase "TimeInForce serialization" $ do
      encode GOOD_TILL_CANCELLED @?= "\"GOOD_TILL_CANCELLED\""
      encode GOOD_TILL_DATE @?= "\"GOOD_TILL_DATE\""
      decode "\"GOOD_TILL_CANCELLED\"" @?= Just GOOD_TILL_CANCELLED

  , testCase "MarketStatus serialization" $ do
      encode TRADEABLE @?= "\"TRADEABLE\""
      encode CLOSED @?= "\"CLOSED\""
      decode "\"TRADEABLE\"" @?= Just TRADEABLE
      decode "\"CLOSED\"" @?= Just CLOSED

  , testCase "IGDealRequest serialization" $ do
      let json = encode sampleDealRequest
      let parsed = decode json :: Maybe IGDealRequest
      parsed @?= Just sampleDealRequest

  , testCase "IGDealResponse serialization" $ do
      let json = encode sampleDealResponse
      let parsed = decode json :: Maybe IGDealResponse
      parsed @?= Just sampleDealResponse

  , testCase "IGDealConfirmation serialization" $ do
      let json = encode sampleDealConfirmation
      let parsed = decode json :: Maybe IGDealConfirmation
      parsed @?= Just sampleDealConfirmation

  , testCase "IGWorkingOrderRequest serialization" $ do
      let json = encode sampleWorkingOrderRequest
      let parsed = decode json :: Maybe IGWorkingOrderRequest
      parsed @?= Just sampleWorkingOrderRequest
  ]

-- Data structure tests
dataStructureTests :: TestTree
dataStructureTests = testGroup "Data Structure Tests"
  [ testCase "IGPosition construction" $ do
      positionDealId samplePosition @?= "DIAAAABBBCCC123"
      positionDirection samplePosition @?= BUY
      positionSize samplePosition @?= 1.0
      positionEpic samplePosition @?= "CS.D.EURUSD.CFD.IP"
      positionMarketStatus samplePosition @?= TRADEABLE

  , testCase "IGDealRequest construction" $ do
      dealEpic sampleDealRequest @?= "CS.D.EURUSD.CFD.IP"
      dealDirection sampleDealRequest @?= BUY
      dealOrderType sampleDealRequest @?= MARKET
      dealSize sampleDealRequest @?= 1.0
      dealStopLevel sampleDealRequest @?= Just 1.0950

  , testCase "IGWorkingOrder construction" $ do
      workingOrderDealId sampleWorkingOrder @?= "W1234"
      workingOrderDirection sampleWorkingOrder @?= BUY
      workingOrderOrderType sampleWorkingOrder @?= LIMIT
      workingOrderLevel sampleWorkingOrder @?= 1.0750
      workingOrderTimeInForce sampleWorkingOrder @?= GOOD_TILL_CANCELLED

  , testCase "IGDealConfirmation construction" $ do
      confirmationDealReference sampleDealConfirmation @?= "D8VEMD26FGQTYPH"
      confirmationDealStatus sampleDealConfirmation @?= Just ACCEPTED
      confirmationDirection sampleDealConfirmation @?= Just BUY
      confirmationSize sampleDealConfirmation @?= Just 1.0
  ]

-- Business logic tests
businessLogicTests :: TestTree
businessLogicTests = testGroup "Business Logic Tests"
  [ testCase "Deal request validation - Market order without level" $ do
      let marketOrder = sampleDealRequest { dealOrderType = MARKET, dealLevel = Nothing }
      dealOrderType marketOrder @?= MARKET
      dealLevel marketOrder @?= Nothing

  , testCase "Deal request validation - Limit order with level" $ do
      let limitOrder = sampleDealRequest { dealOrderType = LIMIT, dealLevel = Just 1.1000 }
      dealOrderType limitOrder @?= LIMIT
      dealLevel limitOrder @?= Just 1.1000

  , testCase "Deal request validation - Stop/Limit levels mutually exclusive" $ do
      let withStopLevel = sampleDealRequest { dealStopLevel = Just 1.0950, dealStopDistance = Nothing }
      let withStopDistance = sampleDealRequest { dealStopLevel = Nothing, dealStopDistance = Just 50.0 }

      dealStopLevel withStopLevel @?= Just 1.0950
      dealStopDistance withStopLevel @?= Nothing

      dealStopLevel withStopDistance @?= Nothing
      dealStopDistance withStopDistance @?= Just 50.0

  , testCase "Deal request validation - Trailing stop requirements" $ do
      let trailingStop = sampleDealRequest
            { dealTrailingStop = Just True
            , dealStopLevel = Nothing  -- Should be Nothing when trailing stop is enabled
            , dealStopDistance = Just 50.0
            , dealTrailingStopIncrement = Just 0.0005
            }

      dealTrailingStop trailingStop @?= Just True
      dealStopLevel trailingStop @?= Nothing
      dealStopDistance trailingStop @?= Just 50.0
      dealTrailingStopIncrement trailingStop @?= Just 0.0005

  , testCase "Working order validation - Good till date requirements" $ do
      let goodTillDateOrder = sampleWorkingOrderRequest
            { workingOrderReqTimeInForce = Just GOOD_TILL_DATE
            , workingOrderReqGoodTillDate = Just "2025-12-31T23:59:59Z"
            }

      workingOrderReqTimeInForce goodTillDateOrder @?= Just GOOD_TILL_DATE
      workingOrderReqGoodTillDate goodTillDateOrder @?= Just "2025-12-31T23:59:59Z"

  , testCase "Working order validation - Good till cancelled" $ do
      let gtcOrder = sampleWorkingOrderRequest
            { workingOrderReqTimeInForce = Just GOOD_TILL_CANCELLED
            , workingOrderReqGoodTillDate = Nothing
            }

      workingOrderReqTimeInForce gtcOrder @?= Just GOOD_TILL_CANCELLED
      workingOrderReqGoodTillDate gtcOrder @?= Nothing
  ]

-- Error handling tests
errorHandlingTests :: TestTree
errorHandlingTests = testGroup "Error Handling Tests"
  [ testCase "Invalid JSON parsing should fail gracefully" $ do
      let invalidJson = L8.pack "{\"invalid\": \"json\"}"
      let parsed = decode invalidJson :: Maybe IGPosition
      parsed @?= Nothing

  , testCase "Missing required fields should fail" $ do
      let incompleteJson = L8.pack "{\"dealId\": \"test\"}"
      let parsed = decode incompleteJson :: Maybe IGPosition
      parsed @?= Nothing

  , testCase "Invalid enum values should fail" $ do
      let invalidDirection = decode "\"INVALID_DIRECTION\"" :: Maybe Direction
      let invalidOrderType = decode "\"INVALID_ORDER\"" :: Maybe OrderType
      let invalidStatus = decode "\"INVALID_STATUS\"" :: Maybe DealStatus

      invalidDirection @?= Nothing
      invalidOrderType @?= Nothing
      invalidStatus @?= Nothing
  ]

-- Complex JSON parsing tests
complexJsonTests :: TestTree
complexJsonTests = testGroup "Complex JSON Parsing Tests"
  [ testCase "IGPosition with nested structure" $ do
      let positionJson = L8.pack $ concat
            [ "{"
            , "  \"position\": {"
            , "    \"dealId\": \"DIAAAABBBCCC123\","
            , "    \"dealReference\": \"REF-001\","
            , "    \"direction\": \"BUY\","
            , "    \"size\": 1.0,"
            , "    \"level\": 1.1005,"
            , "    \"stopLevel\": 1.0950,"
            , "    \"limitLevel\": 1.1050,"
            , "    \"trailingStop\": false,"
            , "    \"currency\": \"USD\","
            , "    \"createdDateUTC\": \"2025-09-07T12:00:00Z\""
            , "  },"
            , "  \"market\": {"
            , "    \"epic\": \"CS.D.EURUSD.CFD.IP\","
            , "    \"instrumentName\": \"EUR/USD\","
            , "    \"bid\": 1.1003,"
            , "    \"offer\": 1.1005,"
            , "    \"marketStatus\": \"TRADEABLE\""
            , "  }"
            , "}"
            ]

      let parsed = decode positionJson :: Maybe IGPosition
      case parsed of
        Nothing -> assertFailure "Failed to parse position JSON"
        Just position -> do
          positionDealId position @?= "DIAAAABBBCCC123"
          positionDirection position @?= BUY
          positionEpic position @?= "CS.D.EURUSD.CFD.IP"
          positionMarketStatus position @?= TRADEABLE

  , testCase "IGWorkingOrder with nested structure" $ do
      let orderJson = L8.pack $ concat
            [ "{"
            , "  \"marketData\": {"
            , "    \"epic\": \"CS.D.EURUSD.CFD.IP\","
            , "    \"instrumentName\": \"EUR/USD\","
            , "    \"bid\": 1.1003,"
            , "    \"offer\": 1.1005,"
            , "    \"marketStatus\": \"TRADEABLE\""
            , "  },"
            , "  \"workingOrderData\": {"
            , "    \"dealId\": \"W1234\","
            , "    \"direction\": \"BUY\","
            , "    \"orderType\": \"LIMIT\","
            , "    \"orderLevel\": 1.0750,"
            , "    \"orderSize\": 1.0,"
            , "    \"timeInForce\": \"GOOD_TILL_CANCELLED\","
            , "    \"createdDateUTC\": \"2025-09-01T10:00:00Z\""
            , "  }"
            , "}"
            ]

      let parsed = decode orderJson :: Maybe IGWorkingOrder
      case parsed of
        Nothing -> assertFailure "Failed to parse working order JSON"
        Just order -> do
          workingOrderDealId order @?= "W1234"
          workingOrderDirection order @?= BUY
          workingOrderOrderType order @?= LIMIT
          workingOrderLevel order @?= 1.0750
          workingOrderTimeInForce order @?= GOOD_TILL_CANCELLED

  , testCase "IGPositionResponse with array" $ do
      let responseJson = L8.pack $ concat
            [ "{"
            , "  \"positions\": ["
            , "    {"
            , "      \"position\": {"
            , "        \"dealId\": \"DEAL1\","
            , "        \"direction\": \"BUY\","
            , "        \"size\": 1.0,"
            , "        \"level\": 1.1005,"
            , "        \"currency\": \"USD\","
            , "        \"createdDateUTC\": \"2025-09-07T12:00:00Z\""
            , "      },"
            , "      \"market\": {"
            , "        \"epic\": \"CS.D.EURUSD.CFD.IP\","
            , "        \"instrumentName\": \"EUR/USD\","
            , "        \"marketStatus\": \"TRADEABLE\""
            , "      }"
            , "    }"
            , "  ]"
            , "}"
            ]

      let parsed = decode responseJson :: Maybe IGPositionResponse
      case parsed of
        Nothing -> assertFailure "Failed to parse position response JSON"
        Just response -> do
          length (positionsData response) @?= 1
          let position = head (positionsData response)
          positionDealId position @?= "DEAL1"
          positionDirection position @?= BUY
  ]

-- All tests combined
dealsTests :: TestTree
dealsTests = testGroup "IG Deals API Tests"
  [ jsonSerializationTests
  , dataStructureTests
  , businessLogicTests
  , errorHandlingTests
  , complexJsonTests
  ]
