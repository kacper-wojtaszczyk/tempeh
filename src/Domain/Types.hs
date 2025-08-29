{-# LANGUAGE DeriveGeneric #-}
module Domain.Types where

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Scientific (Scientific)

newtype Price = Price { unPrice :: Scientific }
  deriving (Show, Eq, Ord, Generic)

newtype Qty = Qty { unQty :: Scientific }
  deriving (Show, Eq, Ord, Generic)

data Side = Buy | Sell
  deriving (Show, Eq, Generic)

data Instrument = EURUSD
  deriving (Show, Eq, Ord, Generic)

data Candle = Candle
  { cTime  :: UTCTime
  , cOpen  :: Price
  , cHigh  :: Price
  , cLow   :: Price
  , cClose :: Price
  } deriving (Show, Eq, Generic)

data Signal = Enter Side | Exit | Hold
  deriving (Show, Eq, Generic)

data Order = Market
  { oInstr :: Instrument
  , oSide  :: Side
  , oQty   :: Qty
  , oPrice :: Maybe Price
  } deriving (Show, Eq, Generic)

newtype OrderId = OrderId Int
  deriving (Show, Eq, Ord, Generic)

data Position = Position
  { pInstr :: Instrument
  , pSide  :: Side
  , pQty   :: Qty
  , pEntry :: Price
  } deriving (Show, Eq, Generic)

data Account = Account
  { aId      :: Text
  , aBalance :: Scientific
  } deriving (Show, Eq, Generic)

data Tick = Tick
  { tTime  :: UTCTime
  , tInstr :: Instrument
  , tBid   :: Price
  , tAsk   :: Price
  } deriving (Show, Eq, Generic)
