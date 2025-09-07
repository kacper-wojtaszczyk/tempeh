{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Adapter.IG.Types
  ( IGSession(..)
  , IGLoginRequest(..)
  , IGLoginResponse(..)
  , IGMarket(..)
  , BrokerConnection(..)
  , SubscriptionState(..)
  , StreamingMode(..)
  -- New deals API types
  , IGPosition(..)
  , IGPositionResponse(..)
  , IGDealRequest(..)
  , IGDealResponse(..)
  , IGDealConfirmation(..)
  , IGWorkingOrder(..)
  , IGWorkingOrderRequest(..)
  , IGWorkingOrderResponse(..)
  , Direction(..)
  , OrderType(..)
  , DealStatus(..)
  , TimeInForce(..)
  , MarketStatus(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), (.:?), object, withObject, withText, (.!=), Value(Null))
import GHC.Generics (Generic)
import Control.Concurrent.STM (TVar)
import Control.Concurrent.Async (Async)
import Data.Map (Map)

import Domain.Types (ConnectionId, ConnectionStatus, Instrument, Tick)
import Util.Config (BrokerConfig)

-- Streaming mode configuration
data StreamingMode = RESTPolling | WebSocketStreaming
  deriving (Show, Eq, Generic)

-- IG Session types
data IGSession = IGSession
  { igSessionToken :: Text
  , igCST :: Text
  , igXSecurityToken :: Text
  , igExpiresAt :: UTCTime
  , igLightstreamerEndpoint :: Maybe Text
  } deriving (Show, Eq, Generic)

-- IG Login types
data IGLoginRequest = IGLoginRequest
  { loginUsername :: Text
  , loginPassword :: Text
  , loginEncryptedPassword :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON IGLoginRequest where
  toJSON req = object
    [ "identifier" .= loginUsername req
    , "password" .= loginPassword req
    , "encryptedPassword" .= loginEncryptedPassword req
    ]

data IGLoginResponse = IGLoginResponse
  { responseLightstreamerEndpoint :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON IGLoginResponse where
  parseJSON = withObject "IGLoginResponse" $ \o -> IGLoginResponse
    <$> o .:? "lightstreamerEndpoint"

-- IG Market data types
data IGMarket = IGMarket
  { marketEpic :: Text
  , marketInstrument :: Text
  , marketBid :: Maybe Double
  , marketAsk :: Maybe Double
  , marketUpdateTime :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON IGMarket where
  parseJSON = withObject "IGMarket" $ \o -> do
    -- Handle nested structure: the IG API returns { "instrument": { ... }, "snapshot": { "bid": ..., "offer": ... } }
    instrumentObj <- o .: "instrument"
    snapshotObj <- o .: "snapshot"

    epic <- instrumentObj .: "epic"
    instrumentName <- instrumentObj .: "name"
    bid <- snapshotObj .:? "bid"
    offer <- snapshotObj .:? "offer"
    updateTime <- snapshotObj .:? "updateTime"

    return $ IGMarket epic instrumentName bid offer updateTime

instance ToJSON IGMarket where
  toJSON market = object
    [ "epic" .= marketEpic market
    , "instrumentName" .= marketInstrument market
    , "bid" .= marketBid market
    , "offer" .= marketAsk market
    , "updateTime" .= marketUpdateTime market
    ]

-- Broker connection types
data BrokerConnection = BrokerConnection
  { bcConnectionId :: ConnectionId
  , bcConfig :: BrokerConfig
  , bcStatus :: TVar ConnectionStatus
  , bcLastHeartbeat :: TVar UTCTime
  , bcSubscriptions :: TVar (Map Instrument SubscriptionState)
  , bcReconnectCount :: TVar Int
  , bcHeartbeatAsync :: Maybe (Async ())
  , bcIGSession :: TVar (Maybe IGSession)
  , bcBufferSize :: Int
  , bcMaxTicksPerSecond :: Double  -- Changed from Int to Double for fractional rates
  , bcStreamingMode :: StreamingMode
  }

data SubscriptionState = SubscriptionState
  { ssTickBuffer :: TVar [Tick]
  , ssStartTime :: UTCTime
  , ssTicksReceived :: TVar Int
  , ssLastTickTime :: TVar (Maybe UTCTime)
  , ssStreamingSubscription :: Maybe Int  -- Lightstreamer subscription ID
  }

-- New types for IG deals API

-- Direction enum
data Direction = BUY | SELL
  deriving (Show, Eq, Generic)

instance ToJSON Direction where
  toJSON BUY = "BUY"
  toJSON SELL = "SELL"

instance FromJSON Direction where
  parseJSON = withText "Direction" $ \t ->
    case t of
      "BUY" -> pure BUY
      "SELL" -> pure SELL
      _ -> fail $ "Unknown direction: " ++ T.unpack t

-- Order type enum
data OrderType = MARKET | LIMIT | STOP | QUOTE
  deriving (Show, Eq, Generic)

instance ToJSON OrderType where
  toJSON MARKET = "MARKET"
  toJSON LIMIT = "LIMIT"
  toJSON STOP = "STOP"
  toJSON QUOTE = "QUOTE"

instance FromJSON OrderType where
  parseJSON = withText "OrderType" $ \t ->
    case t of
      "MARKET" -> pure MARKET
      "LIMIT" -> pure LIMIT
      "STOP" -> pure STOP
      "QUOTE" -> pure QUOTE
      _ -> fail $ "Unknown order type: " ++ T.unpack t

-- Deal status enum
data DealStatus = ACCEPTED | REJECTED | PENDING | AMENDED | DELETED
  deriving (Show, Eq, Generic)

instance ToJSON DealStatus where
  toJSON ACCEPTED = "ACCEPTED"
  toJSON REJECTED = "REJECTED"
  toJSON PENDING = "PENDING"
  toJSON AMENDED = "AMENDED"
  toJSON DELETED = "DELETED"

instance FromJSON DealStatus where
  parseJSON = withText "DealStatus" $ \t ->
    case t of
      "ACCEPTED" -> pure ACCEPTED
      "REJECTED" -> pure REJECTED
      "PENDING" -> pure PENDING
      "AMENDED" -> pure AMENDED
      "DELETED" -> pure DELETED
      _ -> fail $ "Unknown deal status: " ++ T.unpack t

-- Time in force enum
data TimeInForce = EXECUTE_AND_ELIMINATE | FILL_OR_KILL | GOOD_TILL_CANCELLED | GOOD_TILL_DATE
  deriving (Show, Eq, Generic)

instance ToJSON TimeInForce where
  toJSON EXECUTE_AND_ELIMINATE = "EXECUTE_AND_ELIMINATE"
  toJSON FILL_OR_KILL = "FILL_OR_KILL"
  toJSON GOOD_TILL_CANCELLED = "GOOD_TILL_CANCELLED"
  toJSON GOOD_TILL_DATE = "GOOD_TILL_DATE"

instance FromJSON TimeInForce where
  parseJSON = withText "TimeInForce" $ \t ->
    case t of
      "EXECUTE_AND_ELIMINATE" -> pure EXECUTE_AND_ELIMINATE
      "FILL_OR_KILL" -> pure FILL_OR_KILL
      "GOOD_TILL_CANCELLED" -> pure GOOD_TILL_CANCELLED
      "GOOD_TILL_DATE" -> pure GOOD_TILL_DATE
      _ -> fail $ "Unknown time in force: " ++ T.unpack t

-- Market status enum
data MarketStatus = TRADEABLE | CLOSED | EDITS_ONLY | OFFLINE
  deriving (Show, Eq, Generic)

instance ToJSON MarketStatus where
  toJSON TRADEABLE = "TRADEABLE"
  toJSON CLOSED = "CLOSED"
  toJSON EDITS_ONLY = "EDITS_ONLY"
  toJSON OFFLINE = "OFFLINE"

instance FromJSON MarketStatus where
  parseJSON = withText "MarketStatus" $ \t ->
    case t of
      "TRADEABLE" -> pure TRADEABLE
      "CLOSED" -> pure CLOSED
      "EDITS_ONLY" -> pure EDITS_ONLY
      "OFFLINE" -> pure OFFLINE
      _ -> fail $ "Unknown market status: " ++ T.unpack t

-- Position data type
data IGPosition = IGPosition
  { positionDealId :: Text
  , positionDealReference :: Maybe Text
  , positionDirection :: Direction
  , positionSize :: Double
  , positionLevel :: Double
  , positionStopLevel :: Maybe Double
  , positionLimitLevel :: Maybe Double
  , positionTrailingStop :: Bool
  , positionCurrency :: Text
  , positionCreatedDateUTC :: Text
  , positionEpic :: Text
  , positionInstrumentName :: Text
  , positionBid :: Maybe Double
  , positionOffer :: Maybe Double
  , positionMarketStatus :: MarketStatus
  } deriving (Show, Eq, Generic)

instance FromJSON IGPosition where
  parseJSON = withObject "IGPosition" $ \o -> do
    positionObj <- o .: "position"
    marketObj <- o .: "market"

    dealId <- positionObj .: "dealId"
    dealReference <- positionObj .:? "dealReference"
    direction <- positionObj .: "direction"
    size <- positionObj .: "size"
    level <- positionObj .: "level"
    stopLevel <- positionObj .:? "stopLevel"
    limitLevel <- positionObj .:? "limitLevel"
    trailingStop <- positionObj .:? "trailingStop" .!= False
    currency <- positionObj .: "currency"
    createdDate <- positionObj .: "createdDateUTC"

    epic <- marketObj .: "epic"
    instrumentName <- marketObj .: "instrumentName"
    bid <- marketObj .:? "bid"
    offer <- marketObj .:? "offer"
    marketStatus <- marketObj .: "marketStatus"

    return $ IGPosition dealId dealReference direction size level stopLevel limitLevel
                       trailingStop currency createdDate epic instrumentName bid offer marketStatus

-- Position response wrapper
data IGPositionResponse = IGPositionResponse
  { positionsData :: [IGPosition]
  } deriving (Show, Eq, Generic)

instance FromJSON IGPositionResponse where
  parseJSON = withObject "IGPositionResponse" $ \o -> IGPositionResponse
    <$> o .: "positions"

-- Deal request for creating positions
data IGDealRequest = IGDealRequest
  { dealEpic :: Text
  , dealExpiry :: Text
  , dealDirection :: Direction
  , dealSize :: Double
  , dealOrderType :: OrderType
  , dealLevel :: Maybe Double
  , dealQuoteId :: Maybe Text
  , dealCurrencyCode :: Maybe Text
  , dealForceOpen :: Maybe Bool
  , dealGuaranteedStop :: Maybe Bool
  , dealStopLevel :: Maybe Double
  , dealStopDistance :: Maybe Double
  , dealTrailingStop :: Maybe Bool
  , dealTrailingStopIncrement :: Maybe Double
  , dealLimitLevel :: Maybe Double
  , dealLimitDistance :: Maybe Double
  , dealTimeInForce :: Maybe TimeInForce
  , dealReference :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON IGDealRequest where
  toJSON req = object $ filter ((/= Null) . snd)
    [ "epic" .= dealEpic req
    , "expiry" .= dealExpiry req
    , "direction" .= dealDirection req
    , "size" .= dealSize req
    , "orderType" .= dealOrderType req
    , "level" .= dealLevel req
    , "quoteId" .= dealQuoteId req
    , "currencyCode" .= dealCurrencyCode req
    , "forceOpen" .= dealForceOpen req
    , "guaranteedStop" .= dealGuaranteedStop req
    , "stopLevel" .= dealStopLevel req
    , "stopDistance" .= dealStopDistance req
    , "trailingStop" .= dealTrailingStop req
    , "trailingStopIncrement" .= dealTrailingStopIncrement req
    , "limitLevel" .= dealLimitLevel req
    , "limitDistance" .= dealLimitDistance req
    , "timeInForce" .= dealTimeInForce req
    , "dealReference" .= dealReference req
    ]

instance FromJSON IGDealRequest where
  parseJSON = withObject "IGDealRequest" $ \o -> IGDealRequest
    <$> o .: "epic"
    <*> o .: "expiry"
    <*> o .: "direction"
    <*> o .: "size"
    <*> o .: "orderType"
    <*> o .:? "level"
    <*> o .:? "quoteId"
    <*> o .:? "currencyCode"
    <*> o .:? "forceOpen"
    <*> o .:? "guaranteedStop"
    <*> o .:? "stopLevel"
    <*> o .:? "stopDistance"
    <*> o .:? "trailingStop"
    <*> o .:? "trailingStopIncrement"
    <*> o .:? "limitLevel"
    <*> o .:? "limitDistance"
    <*> o .:? "timeInForce"
    <*> o .:? "dealReference"

-- Deal response
data IGDealResponse = IGDealResponse
  { dealResponseReference :: Text
  , dealResponseStatus :: DealStatus
  } deriving (Show, Eq, Generic)

instance FromJSON IGDealResponse where
  parseJSON = withObject "IGDealResponse" $ \o -> IGDealResponse
    <$> o .: "dealReference"
    <*> o .: "dealStatus"

instance ToJSON IGDealResponse where
  toJSON resp = object
    [ "dealReference" .= dealResponseReference resp
    , "dealStatus" .= dealResponseStatus resp
    ]

-- Deal confirmation
data IGDealConfirmation = IGDealConfirmation
  { confirmationDealReference :: Text
  , confirmationDealId :: Maybe Text
  , confirmationEpic :: Text
  , confirmationDealStatus :: DealStatus
  , confirmationDirection :: Direction
  , confirmationSize :: Double
  , confirmationLevel :: Double
  , confirmationTimeStamp :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON IGDealConfirmation where
  parseJSON = withObject "IGDealConfirmation" $ \o -> IGDealConfirmation
    <$> o .: "dealReference"
    <*> o .:? "dealId"
    <*> o .: "epic"
    <*> o .: "dealStatus"
    <*> o .: "direction"
    <*> o .: "size"
    <*> o .: "level"
    <*> o .: "timeStamp"

instance ToJSON IGDealConfirmation where
  toJSON conf = object
    [ "dealReference" .= confirmationDealReference conf
    , "dealId" .= confirmationDealId conf
    , "epic" .= confirmationEpic conf
    , "dealStatus" .= confirmationDealStatus conf
    , "direction" .= confirmationDirection conf
    , "size" .= confirmationSize conf
    , "level" .= confirmationLevel conf
    , "timeStamp" .= confirmationTimeStamp conf
    ]

-- Working order data
data IGWorkingOrder = IGWorkingOrder
  { workingOrderDealId :: Text
  , workingOrderDirection :: Direction
  , workingOrderOrderType :: OrderType
  , workingOrderLevel :: Double
  , workingOrderSize :: Double
  , workingOrderTimeInForce :: TimeInForce
  , workingOrderGoodTillDate :: Maybe Text
  , workingOrderCreatedDateUTC :: Text
  , workingOrderEpic :: Text
  , workingOrderInstrumentName :: Text
  , workingOrderBid :: Maybe Double
  , workingOrderOffer :: Maybe Double
  , workingOrderMarketStatus :: MarketStatus
  } deriving (Show, Eq, Generic)

instance FromJSON IGWorkingOrder where
  parseJSON = withObject "IGWorkingOrder" $ \o -> do
    marketData <- o .: "marketData"
    workingOrderData <- o .: "workingOrderData"

    dealId <- workingOrderData .: "dealId"
    direction <- workingOrderData .: "direction"
    orderType <- workingOrderData .: "orderType"
    orderLevel <- workingOrderData .: "orderLevel"
    orderSize <- workingOrderData .: "orderSize"
    timeInForce <- workingOrderData .: "timeInForce"
    goodTillDate <- workingOrderData .:? "goodTillDate"
    createdDate <- workingOrderData .: "createdDateUTC"

    epic <- marketData .: "epic"
    instrumentName <- marketData .: "instrumentName"
    bid <- marketData .:? "bid"
    offer <- marketData .:? "offer"
    marketStatus <- marketData .: "marketStatus"

    return $ IGWorkingOrder dealId direction orderType orderLevel orderSize timeInForce
                           goodTillDate createdDate epic instrumentName bid offer marketStatus

-- Working order request
data IGWorkingOrderRequest = IGWorkingOrderRequest
  { workingOrderReqEpic :: Text
  , workingOrderReqExpiry :: Text
  , workingOrderReqDirection :: Direction
  , workingOrderReqSize :: Double
  , workingOrderReqOrderType :: OrderType
  , workingOrderReqLevel :: Double
  , workingOrderReqLimitLevel :: Maybe Double
  , workingOrderReqLimitDistance :: Maybe Double
  , workingOrderReqStopLevel :: Maybe Double
  , workingOrderReqStopDistance :: Maybe Double
  , workingOrderReqGuaranteedStop :: Maybe Bool
  , workingOrderReqTimeInForce :: Maybe TimeInForce
  , workingOrderReqGoodTillDate :: Maybe Text
  , workingOrderReqCurrencyCode :: Maybe Text
  , workingOrderReqForceOpen :: Maybe Bool
  , workingOrderReqDealReference :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON IGWorkingOrderRequest where
  toJSON req = object $ filter ((/= Null) . snd)
    [ "epic" .= workingOrderReqEpic req
    , "expiry" .= workingOrderReqExpiry req
    , "direction" .= workingOrderReqDirection req
    , "size" .= workingOrderReqSize req
    , "orderType" .= workingOrderReqOrderType req
    , "level" .= workingOrderReqLevel req
    , "limitLevel" .= workingOrderReqLimitLevel req
    , "limitDistance" .= workingOrderReqLimitDistance req
    , "stopLevel" .= workingOrderReqStopLevel req
    , "stopDistance" .= workingOrderReqStopDistance req
    , "guaranteedStop" .= workingOrderReqGuaranteedStop req
    , "timeInForce" .= workingOrderReqTimeInForce req
    , "goodTillDate" .= workingOrderReqGoodTillDate req
    , "currencyCode" .= workingOrderReqCurrencyCode req
    , "forceOpen" .= workingOrderReqForceOpen req
    , "dealReference" .= workingOrderReqDealReference req
    ]

instance FromJSON IGWorkingOrderRequest where
  parseJSON = withObject "IGWorkingOrderRequest" $ \o -> IGWorkingOrderRequest
    <$> o .: "epic"
    <*> o .: "expiry"
    <*> o .: "direction"
    <*> o .: "size"
    <*> o .: "orderType"
    <*> o .: "level"
    <*> o .:? "limitLevel"
    <*> o .:? "limitDistance"
    <*> o .:? "stopLevel"
    <*> o .:? "stopDistance"
    <*> o .:? "guaranteedStop"
    <*> o .:? "timeInForce"
    <*> o .:? "goodTillDate"
    <*> o .:? "currencyCode"
    <*> o .:? "forceOpen"
    <*> o .:? "dealReference"

-- Working order response wrapper
data IGWorkingOrderResponse = IGWorkingOrderResponse
  { workingOrdersData :: [IGWorkingOrder]
  } deriving (Show, Eq, Generic)

instance FromJSON IGWorkingOrderResponse where
  parseJSON = withObject "IGWorkingOrderResponse" $ \o -> IGWorkingOrderResponse
    <$> o .: "workingOrders"
