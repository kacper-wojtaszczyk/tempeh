{-# LANGUAGE OverloadedStrings #-}
module Domain.Services.LiveDataService where

import Domain.Types
import Util.Error (Result, TempehError)
import Data.Text (Text)
import Data.Scientific (Scientific)
import Control.Concurrent.STM (STM, TVar)

-- Port for live data streaming (domain interface)
class Monad m => LiveDataProvider m where
  -- Connection management
  connect :: m (Result ConnectionId)
  disconnect :: ConnectionId -> m (Result ())
  getConnectionStatus :: ConnectionId -> m (Result ConnectionStatus)

  -- Data subscription and streaming
  subscribeToInstrument :: ConnectionId -> Instrument -> m (Result ())
  unsubscribeFromInstrument :: ConnectionId -> Instrument -> m (Result ())

  -- Live tick streaming (non-blocking)
  getTickStream :: ConnectionId -> Instrument -> m (Result (STM [Tick]))

  -- Data quality monitoring
  getDataQuality :: ConnectionId -> Instrument -> m (Result LiveDataQuality)

-- Port for account information (read-only for now)
class Monad m => AccountProvider m where
  getAccount :: ConnectionId -> m (Result Account)
  getPositions :: ConnectionId -> m (Result [Position])
  getInstrumentDetails :: ConnectionId -> Instrument -> m (Result InstrumentDetails)

-- Instrument metadata
data InstrumentDetails = InstrumentDetails
  { idInstrument :: Instrument
  , idDisplayName :: Text
  , idPipValue :: Scientific
  , idMinTradeSize :: Scientific
  , idMaxTradeSize :: Scientific
  , idMarginRate :: Scientific
  , idMarketHours :: MarketHours
  } deriving (Show, Eq)

-- Live data configuration for domain services
data LiveDataConfig = LiveDataConfig
  { ldcBufferSize :: Int
  , ldcMaxTicksPerSecond :: Int
  , ldcQualityThreshold :: Double
  , ldcHeartbeatInterval :: Double  -- seconds
  } deriving (Show, Eq)
