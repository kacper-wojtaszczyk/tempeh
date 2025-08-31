{-# LANGUAGE OverloadedStrings #-}
module Domain.Services.BacktestService where

import Domain.Types
import Util.Error (Result, AppError(..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime)

-- Pure domain service for backtesting logic
class Monad m => BacktestService m where
  -- Core backtesting operations
  executeBacktest :: BacktestParameters -> StrategyInstance -> [Candle] -> m (Result BacktestResult)
  calculatePerformanceMetrics :: BacktestResult -> Scientific -> m (Result PerformanceMetrics)
  validateBacktestParameters :: BacktestParameters -> m (Result ())

-- Domain types for backtesting
data BacktestParameters = BacktestParameters
  { bpInitialBalance :: Scientific
  , bpPositionSize :: Scientific
  , bpInstrument :: Instrument
  , bpCommission :: Scientific
  , bpSlippage :: Scientific
  } deriving (Show, Eq)

data BacktestResult = BacktestResult
  { brFinalBalance :: Scientific
  , brTotalTrades :: Int
  , brPnL :: Scientific
  , brTrades :: [TradeRecord]
  , brFinalPosition :: Maybe Position
  } deriving (Show, Eq)

data PerformanceMetrics = PerformanceMetrics
  { pmWinRate :: Scientific
  , pmProfitFactor :: Scientific
  , pmMaxDrawdown :: Scientific
  , pmAverageWin :: Scientific
  , pmAverageLoss :: Scientific
  , pmSharpeRatio :: Maybe Scientific
  } deriving (Show, Eq)

data TradeRecord = TradeRecord
  { trTime :: UTCTime
  , trSide :: Side
  , trQty :: Qty
  , trPrice :: Price
  , trType :: TradeType
  } deriving (Show, Eq)

data TradeType = Open | Close deriving (Show, Eq)

-- Strategy abstraction for domain service
data StrategyInstance = StrategyInstance
  { siName :: Text
  , siDescription :: Text
  , siParameters :: StrategyParameters
  , siSignalGenerator :: SignalGenerator
  }

-- Signal generator function type
type SignalGenerator = [Candle] -> StrategyState -> (Signal, StrategyState)

-- Strategy state (opaque to allow different implementations)
data StrategyState = StrategyState
  { ssInternal :: Text  -- JSON or other serializable state
  } deriving (Show, Eq)

-- Enhanced strategy parameters
data StrategyParameters
  = EmaCrossParams
    { ecpFastPeriod :: Int
    , ecpSlowPeriod :: Int
    , ecpSignalThreshold :: Scientific
    }
  | RSIParams
    { rsipPeriod :: Int
    , rsipOverbought :: Scientific
    , rsipOversold :: Scientific
    }
  deriving (Show, Eq)
