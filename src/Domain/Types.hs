{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Domain.Types where

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Scientific (Scientific)

-- Core domain entities (kept pure and focused)
newtype Price = Price { unPrice :: Scientific }
  deriving (Show, Eq, Ord, Read, Generic)

newtype Qty = Qty { unQty :: Scientific }
  deriving (Show, Eq, Ord, Generic)

data Side = Buy | Sell
  deriving (Show, Eq, Generic)

newtype Instrument = Instrument { unInstrument :: Text }
  deriving (Show, Eq, Ord, Generic)

data Candle = Candle
  { cTime  :: UTCTime
  , cOpen  :: Price
  , cHigh  :: Price
  , cLow   :: Price
  , cClose :: Price
  } deriving (Show, Eq, Generic)

-- Enhanced Signal with more semantic meaning
data Signal
  = Enter Side
  | Exit
  | Hold
  | PartialExit Scientific  -- Partial position closure
  | UpdateStopLoss Price    -- Dynamic stop loss adjustment
  deriving (Show, Eq, Generic)

data Order = Order
  { oInstrument :: Instrument
  , oSide  :: Side
  , oQty   :: Qty
  , oType  :: OrderType
  } deriving (Show, Eq, Generic)

data OrderType
  = Market (Maybe Price)     -- Market order with optional limit
  | Limit Price             -- Limit order
  | Stop Price              -- Stop order
  | StopLimit Price Price   -- Stop-limit order
  deriving (Show, Eq, Generic)

newtype OrderId = OrderId Int
  deriving (Show, Eq, Ord, Generic)

data Position = Position
  { pInstr :: Instrument
  , pSide  :: Side
  , pQty   :: Qty
  , pEntry :: Price
  , pStopLoss :: Maybe Price
  , pTakeProfit :: Maybe Price
  } deriving (Show, Eq, Generic)

data Account = Account
  { aId      :: Text
  , aBalance :: Scientific
  , aEquity  :: Scientific
  , aUsedMargin :: Scientific
  , aFreeMargin :: Scientific
  } deriving (Show, Eq, Generic)

data Tick = Tick
  { tTime  :: UTCTime
  , tInstr :: Instrument
  , tBid   :: Price
  , tAsk   :: Price
  , tVolume :: Maybe Scientific
  } deriving (Show, Eq, Generic)

-- Value objects for better domain modeling
data MarketHours = MarketHours
  { mhOpen :: UTCTime
  , mhClose :: UTCTime
  , mhTimeZone :: Text
  } deriving (Show, Eq, Generic)

data CurrencyPair = CurrencyPair
  { cpBase :: Text
  , cpQuote :: Text
  , cpPipValue :: Scientific
  , cpMinSize :: Scientific
  } deriving (Show, Eq, Generic)

-- Risk management types (moved from BacktestOrchestrator to avoid circular imports)
data RiskLimits = RiskLimits
  { rlMaxDrawdown :: Scientific
  , rlMaxPositionSize :: Scientific
  , rlStopLossThreshold :: Scientific
  } deriving (Show, Eq, Generic)
