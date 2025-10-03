{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Domain.Services.BacktestService where

import Domain.Types
import Domain.Strategy (StrategyState)
import Util.Error (Result, AppError(..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import qualified Data.Aeson as JSON

-- Pure domain service for backtesting logic
class Monad m => BacktestService m where
  -- Core backtesting operations - now uses abstract strategy state
  executeBacktest :: BacktestParameters -> StrategyState -> ([Candle] -> StrategyState -> (Signal, StrategyState)) -> [Candle] -> m (Result BacktestResult)
  calculatePerformanceMetrics :: BacktestResult -> Scientific -> m (Result PerformanceMetrics)
  validateBacktestParameters :: BacktestParameters -> m (Result ())

-- Data access service interface
class Monad m => DataProvider m where
  loadTicks :: Instrument -> DateRange -> m (Result [Tick])
  loadCandles :: Instrument -> DateRange -> CandlePeriod -> m (Result [Candle])
  validateDataQuality :: [Tick] -> m (Result DataQualityReport)

-- Risk management service interface
class Monad m => RiskManager m where
  validateRiskLimits :: RiskLimits -> m (Result ())
  checkRiskLimits :: BacktestResult -> RiskLimits -> m (Result ())
  calculateDrawdown :: [(UTCTime, Scientific)] -> m (Result DrawdownAnalysis)
  validatePositionSize :: Position -> Scientific -> m (Result ())
  assessPortfolioRisk :: [Position] -> Account -> m (Result RiskAssessment)

-- Broker service interface
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

-- Data provider types
data DateRange = DateRange
  { drStartYear :: Int
  , drStartMonth :: Int
  , drEndYear :: Int
  , drEndMonth :: Int
  } deriving (Show, Eq, Generic)

instance JSON.ToJSON DateRange where
  toJSON (DateRange startYear startMonth endYear endMonth) =
    JSON.object [ "start" JSON..= JSON.object [ "year" JSON..= startYear, "month" JSON..= startMonth ]
                , "end" JSON..= JSON.object [ "year" JSON..= endYear, "month" JSON..= endMonth ]
                ]

instance JSON.FromJSON DateRange where
  parseJSON = JSON.withObject "DateRange" $ \v -> DateRange
    <$> (v JSON..: "start" >>= \o -> o JSON..: "year")
    <*> (v JSON..: "start" >>= \o -> o JSON..: "month")
    <*> (v JSON..: "end" >>= \o -> o JSON..: "year")
    <*> (v JSON..: "end" >>= \o -> o JSON..: "month")

data CandlePeriod = OneMinute | FiveMinute | FifteenMinute | OneHour | FourHour | Daily
  deriving (Show, Eq)

data DataQualityReport = DataQualityReport
  { dqrTickCount :: Int
  , dqrGapCount :: Int
  , dqrOutlierCount :: Int
  , dqrAverageSpread :: Scientific
  , dqrQualityScore :: Scientific  -- 0-100 score
  } deriving (Show, Eq)

-- Risk management types
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
