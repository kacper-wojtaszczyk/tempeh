{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Application.Env
  ( AppEnv(..)
  , AppM(..)
  , runAppM
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class (MonadIO, liftIO)

import Domain.Services.BacktestService
import Domain.Types
import Domain.Strategy (StrategyState(..))

import Application.ReportingService (ReportGenerator(..))
import Adapter.CsvDataProvider
import Adapter.RiskManagerAdapter
import Adapter.ReportGeneratorAdapter
import Adapter.BacktestEngine

-- Thin application environment bundling concrete adapters
data AppEnv = AppEnv
  { aeCsvProvider :: CsvDataProvider
  , aeRiskManager :: BasicRiskManager
  , aeReportGen   :: ConsoleReportGenerator
  }

newtype AppM a = AppM { unAppM :: ReaderT AppEnv IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv)

runAppM :: AppEnv -> AppM a -> IO a
runAppM env (AppM r) = runReaderT r env

-- DataProvider instance via CsvDataProvider adapter
instance DataProvider AppM where
  loadTicks instr dr = do
    csv <- asks aeCsvProvider
    liftIO $ runReaderT (runCsvDataProvider $ loadTicks instr dr) csv
  loadCandles instr dr period = do
    csv <- asks aeCsvProvider
    liftIO $ runReaderT (runCsvDataProvider $ loadCandles instr dr period) csv
  validateDataQuality ticks = do
    csv <- asks aeCsvProvider
    liftIO $ runReaderT (runCsvDataProvider $ validateDataQuality ticks) csv

-- RiskManager instance via BasicRiskManager adapter
instance RiskManager AppM where
  validateRiskLimits rl = do
    rm <- asks aeRiskManager
    liftIO $ runReaderT (runBasicRiskManager $ validateRiskLimits rl) rm
  checkRiskLimits res rl = do
    rm <- asks aeRiskManager
    liftIO $ runReaderT (runBasicRiskManager $ checkRiskLimits res rl) rm
  calculateDrawdown eq = do
    rm <- asks aeRiskManager
    liftIO $ runReaderT (runBasicRiskManager $ calculateDrawdown eq) rm
  validatePositionSize pos maxSz = do
    rm <- asks aeRiskManager
    liftIO $ runReaderT (runBasicRiskManager $ validatePositionSize pos maxSz) rm
  assessPortfolioRisk positions acct = do
    rm <- asks aeRiskManager
    liftIO $ runReaderT (runBasicRiskManager $ assessPortfolioRisk positions acct) rm

-- ReportGenerator instance via ConsoleReportGenerator adapter
instance ReportGenerator AppM where
  generateReport res metrics cfg = do
    rg <- asks aeReportGen
    liftIO $ runReaderT (runConsoleReportGenerator $ generateReport res metrics cfg) rg
  generateTradeReport trades = do
    rg <- asks aeReportGen
    liftIO $ runReaderT (runConsoleReportGenerator $ generateTradeReport trades) rg
  generatePerformanceChart ec = do
    rg <- asks aeReportGen
    liftIO $ runReaderT (runConsoleReportGenerator $ generatePerformanceChart ec) rg
  exportReport out fmt path = do
    rg <- asks aeReportGen
    liftIO $ runReaderT (runConsoleReportGenerator $ exportReport out fmt path) rg

-- BacktestService instance delegates to BacktestEngine
instance BacktestService AppM where
  executeBacktest params initState sigGen candles = do
    liftIO $ evalStateT (runBacktestEngine $ executeBacktest params initState sigGen candles) dummyState
  calculatePerformanceMetrics res initialBalance =
    liftIO $ evalStateT (runBacktestEngine $ calculatePerformanceMetrics res initialBalance) dummyState
  validateBacktestParameters bp =
    liftIO $ evalStateT (runBacktestEngine $ validateBacktestParameters bp) dummyState

-- Dummy state (BacktestEngine overwrites state internally before use)
{-# INLINE dummyState #-}
dummyState :: BacktestEngineState
dummyState = BacktestEngineState
  { besBalance = 0
  , besPosition = Nothing
  , besTrades = []
  , besStrategyState = StrategyState { unStrategyState = mempty }
  , besEquityCurve = []
  }
