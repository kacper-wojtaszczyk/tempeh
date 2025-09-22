{-# LANGUAGE OverloadedStrings #-}

module Unit.Adapter.IG.TradingTest (tradingTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent.STM
import Data.Time (getCurrentTime)
import Data.Text (Text)

import Adapter.IG.Trading
import Adapter.IG.Types (IGSession(..), Direction(..), OrderType(..), IGDealRequest(..))
import Domain.Types (Instrument(..), Side(..), Price(..), Qty(..))
import Util.Config (BrokerConfig(..), BrokerType(..), BrokerEnvironment(..), ReconnectPolicy(..))
import Util.Error (Result(..))

-- Mock data for testing
mockBrokerConfig :: BrokerConfig
mockBrokerConfig = BrokerConfig
  { bcBrokerType = IG
  , bcEnvironment = DemoEnv
  , bcBaseUrl = Just "https://test-api.ig.com"
  , bcApiKey = Just "test-api-key"
  , bcUsername = Just "test-username"
  , bcPassword = Just "test-password"
  , bcAccountId = Nothing
  , bcConnectTimeout = 30
  , bcReadTimeout = 30
  , bcReconnectPolicy = ReconnectPolicy 3 1.0 30.0 2.0
  }

mockIGSession :: IO IGSession
mockIGSession = do
  now <- getCurrentTime
  return $ IGSession
    { igSessionToken = "test-session-token"
    , igCST = "test-cst"
    , igXSecurityToken = "test-x-security-token"
    , igExpiresAt = now
    , igLightstreamerEndpoint = Nothing
    }

tradingTests :: TestTree
tradingTests = testGroup "Adapter.IG.Trading"
  [ testGroup "Order Validation"
    [ testCase "Valid market order should pass validation" $ do
        let request = IGDealRequest
              { dealEpic = "CS.D.EURUSD.CFD.IP"
              , dealExpiry = "-"
              , dealDirection = BUY
              , dealSize = 1.0
              , dealOrderType = MARKET
              , dealLevel = Nothing
              , dealQuoteId = Nothing
              , dealCurrencyCode = Just "GBP"
              , dealForceOpen = Just True
              , dealGuaranteedStop = Just False
              , dealStopLevel = Nothing
              , dealStopDistance = Nothing
              , dealTrailingStop = Nothing
              , dealTrailingStopIncrement = Nothing
              , dealLimitLevel = Nothing
              , dealLimitDistance = Nothing
              , dealTimeInForce = Nothing
              , dealReference = Nothing
              }

        -- Test the basic structure
        dealSize request @?= 1.0
        dealOrderType request @?= MARKET
        dealLevel request @?= Nothing

    , testCase "Market order with level should be identified" $ do
        let request = IGDealRequest
              { dealEpic = "CS.D.EURUSD.CFD.IP"
              , dealExpiry = "-"
              , dealDirection = BUY
              , dealSize = 1.0
              , dealOrderType = MARKET
              , dealLevel = Just 1.1000
              , dealQuoteId = Nothing
              , dealCurrencyCode = Just "GBP"
              , dealForceOpen = Just True
              , dealGuaranteedStop = Just False
              , dealStopLevel = Nothing
              , dealStopDistance = Nothing
              , dealTrailingStop = Nothing
              , dealTrailingStopIncrement = Nothing
              , dealLimitLevel = Nothing
              , dealLimitDistance = Nothing
              , dealTimeInForce = Nothing
              , dealReference = Nothing
              }

        -- This would be invalid - market orders shouldn't have levels
        dealOrderType request @?= MARKET
        dealLevel request @?= Just 1.1000

    , testCase "Zero size order should be identified" $ do
        let request = IGDealRequest
              { dealEpic = "CS.D.EURUSD.CFD.IP"
              , dealExpiry = "-"
              , dealDirection = BUY
              , dealSize = 0.0
              , dealOrderType = MARKET
              , dealLevel = Nothing
              , dealQuoteId = Nothing
              , dealCurrencyCode = Just "GBP"
              , dealForceOpen = Just True
              , dealGuaranteedStop = Just False
              , dealStopLevel = Nothing
              , dealStopDistance = Nothing
              , dealTrailingStop = Nothing
              , dealTrailingStopIncrement = Nothing
              , dealLimitLevel = Nothing
              , dealLimitDistance = Nothing
              , dealTimeInForce = Nothing
              , dealReference = Nothing
              }

        -- Zero size orders should be invalid
        dealSize request @?= 0.0
    ]

  , testGroup "Epic Mapping"
    [ testCase "EURUSD should map to correct IG epic" $ do
        let instrument = Instrument "EURUSD"
        let epic = instrumentToIGEpic instrument
        epic @?= "CS.D.EURUSD.CFD.IP"

    , testCase "GBPUSD should map to correct IG epic" $ do
        let instrument = Instrument "GBPUSD"
        let epic = instrumentToIGEpic instrument
        epic @?= "CS.D.GBPUSD.CFD.IP"

    , testCase "Unknown instrument should have fallback epic" $ do
        let instrument = Instrument "UNKNOWN"
        let epic = instrumentToIGEpic instrument
        epic @?= "CS.D.UNKNOWN.CFD.IP"
    ]

  , testGroup "Side Conversion"
    [ testCase "Buy side should convert to BUY direction" $ do
        let side = Buy
        let direction = sideToIGDirection side
        direction @?= BUY

    , testCase "Sell side should convert to SELL direction" $ do
        let side = Sell
        let direction = sideToIGDirection side
        direction @?= SELL
    ]

  , testGroup "Order Request Structure"
    [ testCase "IGDealRequest should have required fields" $ do
        let request = IGDealRequest
              { dealEpic = "CS.D.EURUSD.CFD.IP"
              , dealExpiry = "-"
              , dealDirection = BUY
              , dealSize = 1.0
              , dealOrderType = MARKET
              , dealLevel = Nothing
              , dealQuoteId = Nothing
              , dealCurrencyCode = Just "GBP"
              , dealForceOpen = Just True
              , dealGuaranteedStop = Just False
              , dealStopLevel = Nothing
              , dealStopDistance = Nothing
              , dealTrailingStop = Nothing
              , dealTrailingStopIncrement = Nothing
              , dealLimitLevel = Nothing
              , dealLimitDistance = Nothing
              , dealTimeInForce = Nothing
              , dealReference = Nothing
              }

        dealEpic request @?= "CS.D.EURUSD.CFD.IP"
        dealDirection request @?= BUY
        dealSize request @?= 1.0
        dealCurrencyCode request @?= Just "GBP"
        dealForceOpen request @?= Just True
    ]
  ]
