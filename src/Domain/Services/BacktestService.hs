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

-- Data access service interface (moved from Port.DataProvider)
class Monad m => DataProvider m where
  loadTicks :: Instrument -> DateRange -> m (Result [Tick])
  loadCandles :: Instrument -> DateRange -> CandlePeriod -> m (Result [Candle])
  validateDataQuality :: [Tick] -> m (Result DataQualityReport)

-- Risk management service interface (moved from Port.RiskManager)
class Monad m => RiskManager m where
  validateRiskLimits :: RiskLimits -> m (Result ())
  checkRiskLimits :: BacktestResult -> RiskLimits -> m (Result ())
  calculateDrawdown :: [(UTCTime, Scientific)] -> m (Result DrawdownAnalysis)
  validatePositionSize :: Position -> Scientific -> m (Result ())
  assessPortfolioRisk :: [Position] -> Account -> m (Result RiskAssessment)

-- Broker service interface (moved from Port.Broker)
class Monad m => BrokerService m where
  getAccount :: m Account
  marketOrder :: Order -> m OrderId
  streamTicks :: Instrument -> m [Tick]  -- Simplified from STM for now

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

-- Data provider types (moved from Port.DataProvider)
data DateRange = DateRange
  { drStartYear :: Int
  , drStartMonth :: Int
  , drEndYear :: Int
  , drEndMonth :: Int
  } deriving (Show, Eq)

data CandlePeriod = OneMinute | FiveMinute | FifteenMinute | OneHour | FourHour | Daily
  deriving (Show, Eq)

data DataQualityReport = DataQualityReport
  { dqrTickCount :: Int
  , dqrGapCount :: Int
  , dqrOutlierCount :: Int
  , dqrAverageSpread :: Scientific
  , dqrQualityScore :: Scientific  -- 0-100 score
  } deriving (Show, Eq)

-- Risk management types (moved from Port.RiskManager)
data DrawdownAnalysis = DrawdownAnalysis
  { daMaxDrawdown :: Scientific
  , daMaxDrawdownPercent :: Scientific
  , daDrawdownPeriods :: [DrawdownPeriod]
  , daCurrentDrawdown :: Scientific
  , daRecoveryTime :: Maybe Scientific  -- Days to recover from max drawdown
  } deriving (Show)

data DrawdownPeriod = DrawdownPeriod
  { dpStart :: UTCTime
  , dpEnd :: Maybe UTCTime  -- Nothing if still in drawdown
  , dpMaxDrawdown :: Scientific
  , dpDuration :: Scientific  -- Days
  } deriving (Show)

data RiskAssessment = RiskAssessment
  { raOverallRisk :: RiskLevel
  , raPositionSizing :: RiskLevel
  , raDrawdownRisk :: RiskLevel
  , raLeverageRisk :: RiskLevel
  , raRecommendations :: [Text]
  } deriving (Show)

data RiskLevel = Low | Medium | High | Critical
  deriving (Show, Eq, Ord)
