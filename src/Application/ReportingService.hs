{-# LANGUAGE OverloadedStrings #-}
module Application.ReportingService where

import Domain.Types
import Domain.Services.BacktestService
import Util.Error (Result)
import Data.Scientific (Scientific)
import Data.Time (UTCTime)
import qualified Data.Text as T
import qualified Data.Aeson as JSON

-- Context passed to reporting so we avoid placeholders
data ReportContext = ReportContext
  { rcInstrument :: Instrument
  , rcDateRange :: DateRange
  , rcStrategy :: StrategyParameters
  , rcInitialBalance :: Scientific
  }

-- Application-level reporting service
class Monad m => ReportGenerator m where
  generateReport :: BacktestResult -> PerformanceMetrics -> ReportContext -> m (Result ReportOutput)
  generateTradeReport :: [TradeRecord] -> m (Result T.Text)
  generatePerformanceChart :: [(UTCTime, Scientific)] -> m (Result ChartData)
  exportReport :: ReportOutput -> ReportFormat -> FilePath -> m (Result ())

-- Application-level reporting types
data ReportOutput = ReportOutput
  { roSummary :: ReportSummary
  , roTradeAnalysis :: TradeAnalysis
  , roPerformanceMetrics :: PerformanceMetrics
  , roEquityCurve :: [(UTCTime, Scientific)]
  , roGeneratedAt :: UTCTime
  } deriving (Show)

instance JSON.ToJSON ReportOutput where
  toJSON ro = JSON.object
    [ ("summary", JSON.toJSON (roSummary ro))
    , ("tradeAnalysis", JSON.toJSON (roTradeAnalysis ro))
    , ("performanceMetrics", performanceToJSON (roPerformanceMetrics ro))
    , ("equityCurve", JSON.toJSON (roEquityCurve ro))
    , ("generatedAt", JSON.toJSON (roGeneratedAt ro))
    ]

-- Summary shown to the user
data ReportSummary = ReportSummary
  { rsInstrument :: Instrument
  , rsDateRange :: T.Text
  , rsStrategy :: T.Text
  , rsTotalReturn :: Scientific
  , rsAnnualizedReturn :: Scientific
  , rsFinalBalance :: Scientific
  } deriving (Show)

instance JSON.ToJSON ReportSummary where
  toJSON rs = JSON.object
    [ ("instrument", JSON.toJSON (unInstrument (rsInstrument rs)))
    , ("dateRange", JSON.toJSON (rsDateRange rs))
    , ("strategy", JSON.toJSON (rsStrategy rs))
    , ("totalReturn", JSON.toJSON (rsTotalReturn rs))
    , ("annualizedReturn", JSON.toJSON (rsAnnualizedReturn rs))
    , ("finalBalance", JSON.toJSON (rsFinalBalance rs))
    ]

-- Trade analytics
data TradeAnalysis = TradeAnalysis
  { taTotalTrades :: Int
  , taWinningTrades :: Int
  , taLosingTrades :: Int
  , taLargestWin :: Scientific
  , taLargestLoss :: Scientific
  , taConsecutiveWins :: Int
  , taConsecutiveLosses :: Int
  , taAverageTradeTime :: Scientific  -- in minutes
  } deriving (Show)

instance JSON.ToJSON TradeAnalysis where
  toJSON ta = JSON.object
    [ ("totalTrades", JSON.toJSON (taTotalTrades ta))
    , ("winningTrades", JSON.toJSON (taWinningTrades ta))
    , ("losingTrades", JSON.toJSON (taLosingTrades ta))
    , ("largestWin", JSON.toJSON (taLargestWin ta))
    , ("largestLoss", JSON.toJSON (taLargestLoss ta))
    , ("consecutiveWins", JSON.toJSON (taConsecutiveWins ta))
    , ("consecutiveLosses", JSON.toJSON (taConsecutiveLosses ta))
    , ("averageTradeTime", JSON.toJSON (taAverageTradeTime ta))
    ]

-- Optional chart data
data ChartData = ChartData
  { cdEquityCurve :: [(UTCTime, Scientific)]
  , cdDrawdownCurve :: [(UTCTime, Scientific)]
  , cdTradePoints :: [(UTCTime, Scientific, TradeType)]
  } deriving (Show)

performanceToJSON :: PerformanceMetrics -> JSON.Value
performanceToJSON pm = JSON.object
  [ ("winRate", JSON.toJSON (pmWinRate pm))
  , ("profitFactor", JSON.toJSON (pmProfitFactor pm))
  , ("maxDrawdown", JSON.toJSON (pmMaxDrawdown pm))
  , ("averageWin", JSON.toJSON (pmAverageWin pm))
  , ("averageLoss", JSON.toJSON (pmAverageLoss pm))
  , ("sharpeRatio", maybe JSON.Null JSON.toJSON (pmSharpeRatio pm))
  ]

data ReportFormat = HTML | PDF | CSV | JSON
  deriving (Show, Eq)
