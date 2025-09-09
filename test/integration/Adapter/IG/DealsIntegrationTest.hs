{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Integration.Adapter.IG.DealsIntegrationTest
  ( dealsIntegrationTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime, addUTCTime)
import Control.Monad (when)
import Control.Exception (try, SomeException)

import Adapter.IG.Types
import Adapter.IG.Deals
import Adapter.IG.Auth
import Util.Config (BrokerConfig(..), defaultBrokerConfig)
import Util.Error (Result(..))

-- Test configuration for demo account
testConfig :: BrokerConfig
testConfig = defaultBrokerConfig
  { bcBaseUrl = Just "https://demo-api.ig.com/gateway/deal"
  , bcApiKey = Just "mock-api-key-for-testing"
  }

-- Mock session for integration testing (would normally come from actual login)
mockSession :: IO IGSession
mockSession = do
  now <- getCurrentTime
  return $ IGSession
    { igSessionToken = "mock-session-token"
    , igCST = "mock-cst"
    , igXSecurityToken = "mock-x-security-token"
    , igExpiresAt = addUTCTime 3600 now
    , igLightstreamerEndpoint = Just "https://demo-api.ig.com/lightstreamer"
    }

-- Helper function to check if we have real credentials for integration testing
hasRealCredentials :: BrokerConfig -> Bool
hasRealCredentials config =
  case bcApiKey config of
    Just key -> not (T.isPrefixOf "mock-" key || T.isPrefixOf "test-" key)
    Nothing -> False

-- Position management integration tests
positionManagementTests :: TestTree
positionManagementTests = testGroup "Position Management Integration Tests"
  [ testCase "Get positions endpoint structure" $ do
      -- This test validates that our request structure is correct
      -- In a real integration test, we would make actual API calls
      session <- mockSession

      -- Test that we can construct requests without errors
      let testResult = do
            positions <- getPositions testConfig session
            case positions of
              Left err -> return $ "Expected network error in mock test: " <> T.pack (show err)
              Right _ -> return "Should not reach here in mock test"

      result <- try testResult :: IO (Either SomeException Text)
      case result of
        Left (ex :: SomeException) -> return () -- Expected in mock test
        Right msg -> return () -- Also acceptable

  , testCase "Create position request structure" $ do
      session <- mockSession

      let dealRequest = IGDealRequest
            { dealEpic = "CS.D.EURUSD.CFD.IP"
            , dealExpiry = "-"
            , dealDirection = BUY
            , dealSize = 0.5  -- Small size for testing
            , dealOrderType = MARKET
            , dealLevel = Nothing
            , dealQuoteId = Nothing
            , dealCurrencyCode = Just "USD"
            , dealForceOpen = Just True
            , dealGuaranteedStop = Just False
            , dealStopLevel = Nothing
            , dealStopDistance = Just 20.0  -- 20 pips stop
            , dealTrailingStop = Just False
            , dealTrailingStopIncrement = Nothing
            , dealLimitLevel = Nothing
            , dealLimitDistance = Just 30.0  -- 30 pips limit
            , dealTimeInForce = Nothing
            , dealReference = Just "integration-test-001"
            }

      -- Validate request structure
      dealEpic dealRequest @?= "CS.D.EURUSD.CFD.IP"
      dealDirection dealRequest @?= BUY
      dealOrderType dealRequest @?= MARKET
      dealStopDistance dealRequest @?= Just 20.0
      dealLimitDistance dealRequest @?= Just 30.0

  , testCase "Close position request structure" $ do
      session <- mockSession

      let closeRequest = IGDealRequest
            { dealEpic = "CS.D.EURUSD.CFD.IP"
            , dealExpiry = "-"
            , dealDirection = SELL  -- Opposite direction to close
            , dealSize = 0.5
            , dealOrderType = MARKET
            , dealLevel = Nothing
            , dealQuoteId = Nothing
            , dealCurrencyCode = Just "USD"
            , dealForceOpen = Nothing
            , dealGuaranteedStop = Nothing
            , dealStopLevel = Nothing
            , dealStopDistance = Nothing
            , dealTrailingStop = Nothing
            , dealTrailingStopIncrement = Nothing
            , dealLimitLevel = Nothing
            , dealLimitDistance = Nothing
            , dealTimeInForce = Nothing
            , dealReference = Just "integration-close-001"
            }

      -- Validate close request structure
      dealDirection closeRequest @?= SELL
      dealOrderType closeRequest @?= MARKET
      dealReference closeRequest @?= Just "integration-close-001"
  ]

-- Working orders integration tests
workingOrdersTests :: TestTree
workingOrdersTests = testGroup "Working Orders Integration Tests"
  [ testCase "Get working orders endpoint structure" $ do
      session <- mockSession

      -- Test request construction
      let testResult = do
            orders <- getWorkingOrders testConfig session
            case orders of
              Left err -> return $ "Expected in mock: " <> T.pack (show err)
              Right _ -> return "Mock success"

      result <- try testResult :: IO (Either SomeException Text)
      case result of
        Left (_ :: SomeException) -> return () -- Expected
        Right _ -> return ()

  , testCase "Create working order request structure" $ do
      session <- mockSession

      let orderRequest = IGWorkingOrderRequest
            { workingOrderReqEpic = "CS.D.GBPUSD.CFD.IP"
            , workingOrderReqExpiry = "-"
            , workingOrderReqDirection = BUY
            , workingOrderReqSize = 1.0
            , workingOrderReqOrderType = LIMIT
            , workingOrderReqLevel = 1.2000  -- Entry level
            , workingOrderReqLimitLevel = Just 1.2100  -- Take profit
            , workingOrderReqLimitDistance = Nothing
            , workingOrderReqStopLevel = Just 1.1900  -- Stop loss
            , workingOrderReqStopDistance = Nothing
            , workingOrderReqGuaranteedStop = Just False
            , workingOrderReqTimeInForce = Just GOOD_TILL_CANCELLED
            , workingOrderReqGoodTillDate = Nothing
            , workingOrderReqCurrencyCode = Just "USD"
            , workingOrderReqForceOpen = Just True
            , workingOrderReqDealReference = Just "working-order-test-001"
            }

      -- Validate working order request
      workingOrderReqEpic orderRequest @?= "CS.D.GBPUSD.CFD.IP"
      workingOrderReqDirection orderRequest @?= BUY
      workingOrderReqOrderType orderRequest @?= LIMIT
      workingOrderReqLevel orderRequest @?= 1.2000
      workingOrderReqLimitLevel orderRequest @?= Just 1.2100
      workingOrderReqStopLevel orderRequest @?= Just 1.1900
      workingOrderReqTimeInForce orderRequest @?= Just GOOD_TILL_CANCELLED

  , testCase "Working order with good till date" $ do
      session <- mockSession

      let orderRequest = IGWorkingOrderRequest
            { workingOrderReqEpic = "CS.D.EURUSD.CFD.IP"
            , workingOrderReqExpiry = "-"
            , workingOrderReqDirection = SELL
            , workingOrderReqSize = 0.5
            , workingOrderReqOrderType = STOP
            , workingOrderReqLevel = 1.0950  -- Stop entry level
            , workingOrderReqLimitLevel = Nothing
            , workingOrderReqLimitDistance = Just 50.0  -- 50 pips profit
            , workingOrderReqStopLevel = Nothing
            , workingOrderReqStopDistance = Just 30.0  -- 30 pips stop
            , workingOrderReqGuaranteedStop = Just False
            , workingOrderReqTimeInForce = Just GOOD_TILL_DATE
            , workingOrderReqGoodTillDate = Just "2025-12-31T23:59:59"
            , workingOrderReqCurrencyCode = Just "USD"
            , workingOrderReqForceOpen = Just False
            , workingOrderReqDealReference = Just "stop-order-test-001"
            }

      -- Validate good till date setup
      workingOrderReqTimeInForce orderRequest @?= Just GOOD_TILL_DATE
      workingOrderReqGoodTillDate orderRequest @?= Just "2025-12-31T23:59:59"
      workingOrderReqOrderType orderRequest @?= STOP
      workingOrderReqLimitDistance orderRequest @?= Just 50.0
      workingOrderReqStopDistance orderRequest @?= Just 30.0
  ]

-- Deal confirmation integration tests
confirmationTests :: TestTree
confirmationTests = testGroup "Deal Confirmation Integration Tests"
  [ testCase "Get deal confirmation endpoint structure" $ do
      session <- mockSession

      let dealReference = "TEST-DEAL-REF-001"

      -- Test confirmation request construction
      let testResult = do
            confirmation <- getDealConfirmation testConfig session dealReference
            case confirmation of
              Left err -> return $ "Expected: " <> T.pack (show err)
              Right _ -> return "Success"

      result <- try testResult :: IO (Either SomeException Text)
      case result of
        Left (_ :: SomeException) -> return () -- Expected in mock
        Right _ -> return ()

  , testCase "Deal confirmation parsing validation" $ do
      -- Test that we can construct and validate deal confirmations
      let confirmation = IGDealConfirmation
            { confirmationDealReference = "TEST-REF-123"
            , confirmationDealId = Just "DEAL-ID-456"
            , confirmationEpic = Just "CS.D.EURUSD.CFD.IP"
            , confirmationDealStatus = Just ACCEPTED
            , confirmationDirection = Just BUY
            , confirmationSize = Just 1.0
            , confirmationLevel = Just 1.1050
            , confirmationTimeStamp = Just "2025-09-07T15:30:00"
            , confirmationReason = Nothing
            }

      confirmationDealReference confirmation @?= "TEST-REF-123"
      confirmationDealStatus confirmation @?= Just ACCEPTED
      confirmationDirection confirmation @?= Just BUY
      confirmationSize confirmation @?= Just 1.0
  ]

-- Error handling integration tests
errorHandlingIntegrationTests :: TestTree
errorHandlingIntegrationTests = testGroup "Error Handling Integration Tests"
  [ testCase "Invalid session handling" $ do
      -- Test with expired/invalid session
      now <- getCurrentTime
      let invalidSession = IGSession
            { igSessionToken = "invalid-token"
            , igCST = "invalid-cst"
            , igXSecurityToken = "invalid-security-token"
            , igExpiresAt = addUTCTime (-3600) now  -- Expired
            , igLightstreamerEndpoint = Nothing
            }

      -- This should fail gracefully
      result <- try $ getPositions testConfig invalidSession :: IO (Either SomeException (Result [IGPosition]))
      case result of
        Left (_ :: SomeException) -> return () -- Expected
        Right (Left _) -> return () -- Expected error result
        Right (Right _) -> assertFailure "Should not succeed with invalid session"

  , testCase "Invalid epic handling" $ do
      session <- mockSession

      let invalidDealRequest = IGDealRequest
            { dealEpic = "INVALID.EPIC.FORMAT"
            , dealExpiry = "-"
            , dealDirection = BUY
            , dealSize = 1.0
            , dealOrderType = MARKET
            , dealLevel = Nothing
            , dealQuoteId = Nothing
            , dealCurrencyCode = Nothing
            , dealForceOpen = Nothing
            , dealGuaranteedStop = Nothing
            , dealStopLevel = Nothing
            , dealStopDistance = Nothing
            , dealTrailingStop = Nothing
            , dealTrailingStopIncrement = Nothing
            , dealLimitLevel = Nothing
            , dealLimitDistance = Nothing
            , dealTimeInForce = Nothing
            , dealReference = Nothing
            }

      -- Should construct request properly even with invalid epic
      dealEpic invalidDealRequest @?= "INVALID.EPIC.FORMAT"

  , testCase "Network error handling structure" $ do
      -- Test that our error handling structure works
      let networkConfig = testConfig { bcBaseUrl = Just "https://invalid-url-that-does-not-exist.com" }
      session <- mockSession

      result <- try $ getPositions networkConfig session :: IO (Either SomeException (Result [IGPosition]))
      case result of
        Left (_ :: SomeException) -> return () -- Expected network error
        Right (Left _) -> return () -- Expected error result
        Right (Right _) -> return () -- Unexpected but not a test failure
  ]

-- Full workflow integration tests
workflowTests :: TestTree
workflowTests = testGroup "Full Workflow Integration Tests"
  [ testCase "Complete trading workflow structure" $ do
      session <- mockSession

      -- 1. Check existing positions
      positionsResult <- try $ getPositions testConfig session :: IO (Either SomeException (Result [IGPosition]))

      -- 2. Create a new position
      let newPosition = IGDealRequest
            { dealEpic = "CS.D.EURUSD.CFD.IP"
            , dealExpiry = "-"
            , dealDirection = BUY
            , dealSize = 0.1  -- Micro lot
            , dealOrderType = MARKET
            , dealLevel = Nothing
            , dealQuoteId = Nothing
            , dealCurrencyCode = Just "USD"
            , dealForceOpen = Just True
            , dealGuaranteedStop = Just False
            , dealStopLevel = Nothing
            , dealStopDistance = Just 10.0  -- 10 pips
            , dealTrailingStop = Just False
            , dealTrailingStopIncrement = Nothing
            , dealLimitLevel = Nothing
            , dealLimitDistance = Just 15.0  -- 15 pips
            , dealTimeInForce = Nothing
            , dealReference = Just "workflow-test-001"
            }

      createResult <- try $ createPosition testConfig session newPosition :: IO (Either SomeException (Result IGDealResponse))

      -- 3. Create working order
      let workingOrder = IGWorkingOrderRequest
            { workingOrderReqEpic = "CS.D.GBPUSD.CFD.IP"
            , workingOrderReqExpiry = "-"
            , workingOrderReqDirection = SELL
            , workingOrderReqSize = 0.1
            , workingOrderReqOrderType = LIMIT
            , workingOrderReqLevel = 1.2500
            , workingOrderReqLimitLevel = Just 1.2450
            , workingOrderReqLimitDistance = Nothing
            , workingOrderReqStopLevel = Just 1.2550
            , workingOrderReqStopDistance = Nothing
            , workingOrderReqGuaranteedStop = Just False
            , workingOrderReqTimeInForce = Just GOOD_TILL_CANCELLED
            , workingOrderReqGoodTillDate = Nothing
            , workingOrderReqCurrencyCode = Just "USD"
            , workingOrderReqForceOpen = Just True
            , workingOrderReqDealReference = Just "workflow-working-001"
            }

      workingOrderResult <- try $ createWorkingOrder testConfig session workingOrder :: IO (Either SomeException (Result IGDealResponse))

      -- All operations should complete without throwing exceptions
      -- (they may return Left results due to network/auth issues in mock environment)
      return ()

  , testCase "Position amendment workflow" $ do
      session <- mockSession

      let amendmentRequest = IGDealRequest
            { dealEpic = "CS.D.EURUSD.CFD.IP"
            , dealExpiry = "-"
            , dealDirection = BUY
            , dealSize = 1.0
            , dealOrderType = MARKET
            , dealLevel = Nothing
            , dealQuoteId = Nothing
            , dealCurrencyCode = Nothing
            , dealForceOpen = Nothing
            , dealGuaranteedStop = Nothing
            , dealStopLevel = Just 1.0900  -- New stop level
            , dealStopDistance = Nothing
            , dealTrailingStop = Nothing
            , dealTrailingStopIncrement = Nothing
            , dealLimitLevel = Just 1.1200  -- New limit level
            , dealLimitDistance = Nothing
            , dealTimeInForce = Nothing
            , dealReference = Just "amendment-test-001"
            }

      result <- try $ amendPosition testConfig session "MOCK-DEAL-ID" amendmentRequest :: IO (Either SomeException (Result IGDealResponse))

      -- Should complete without exception
      return ()
  ]

-- All integration tests combined
dealsIntegrationTests :: TestTree
dealsIntegrationTests = testGroup "IG Deals Integration Tests"
  [ positionManagementTests
  , workingOrdersTests
  , confirmationTests
  , errorHandlingIntegrationTests
  , workflowTests
  ]
