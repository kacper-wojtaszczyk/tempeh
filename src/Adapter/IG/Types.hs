{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Adapter.IG.Types
  ( IGSession(..)
  , IGLoginRequest(..)
  , IGLoginResponse(..)
  , IGMarket(..)
  , BrokerConnection(..)
  , SubscriptionState(..)
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), (.:?), object, withObject)
import GHC.Generics (Generic)
import Control.Concurrent.STM (TVar)
import Control.Concurrent.Async (Async)
import Data.Map (Map)

import Domain.Types (ConnectionId, ConnectionStatus, Instrument, Tick)
import Util.Config (BrokerConfig)

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
  parseJSON = withObject "IGMarket" $ \o -> IGMarket
    <$> o .: "epic"
    <*> o .: "instrumentName"
    <*> o .:? "bid"
    <*> o .:? "offer"
    <*> o .:? "updateTime"

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
  , bcMaxTicksPerSecond :: Int
  }

data SubscriptionState = SubscriptionState
  { ssTickBuffer :: TVar [Tick]
  , ssStartTime :: UTCTime
  , ssTicksReceived :: TVar Int
  , ssLastTickTime :: TVar (Maybe UTCTime)
  }
