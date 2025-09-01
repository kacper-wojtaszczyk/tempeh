{-# LANGUAGE OverloadedStrings #-}
module Application.ReportingService where

import Domain.Types
import Domain.Services.BacktestService
import Util.Error (Result)
import Data.Scientific (Scientific)
import Data.Time (UTCTime)
import qualified Data.Text as T

-- Application-level reporting service
class Monad m => ReportGenerator m where
  generateReport :: BacktestResult -> PerformanceMetrics -> () -> m (Result ReportOutput)
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

data ReportSummary = ReportSummary
  { rsInstrument :: Instrument
  , rsDateRange :: T.Text
  , rsStrategy :: T.Text
  , rsTotalReturn :: Scientific
  , rsAnnualizedReturn :: Scientific
  , rsFinalBalance :: Scientific
  } deriving (Show)

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

data ChartData = ChartData
  { cdEquityCurve :: [(UTCTime, Scientific)]
  , cdDrawdownCurve :: [(UTCTime, Scientific)]
  , cdTradePoints :: [(UTCTime, Scientific, TradeType)]
  } deriving (Show)

data ReportFormat = HTML | PDF | CSV | JSON
  deriving (Show, Eq)
