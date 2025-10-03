{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Application.Env
  ( AppEnv(..)
  , AppM(..)
  , runAppM
  , runAppMWithLogging
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Domain.Services.BacktestService
import Domain.Types
import Domain.Strategy (StrategyState(..))

import Application.ReportingService (ReportGenerator(..))
import Adapter.CsvDataProvider
import Adapter.RiskManagerAdapter
import Adapter.ReportGeneratorAdapter
import Adapter.BacktestEngine

-- Import our new logging and error handling
import Util.Logger (MonadLogger(..), LogContext(..), ComponentName(..), CorrelationId(..), runFileLoggerWithComponent, emptyLogContext, logInfo, logError, logWarn, logDebug)
import Util.Error (TempehError, Result, AppError(..), dataError, strategyError, configError, systemError)
import qualified Data.Aeson as JSON

-- Enhanced application environment with logging context
data AppEnv = AppEnv
  { aeCsvProvider :: CsvDataProvider
  , aeRiskManager :: BasicRiskManager
  , aeReportGen   :: ConsoleReportGenerator
  , aeLogContext  :: LogContext  -- New: logging context
  }

-- Enhanced AppM with logging capability
newtype AppM a = AppM { unAppM :: ReaderT AppEnv IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv)

-- Make AppM a logging monad
instance MonadLogger AppM where
  logAtLevel level msg metadata = do
    ctx <- asks aeLogContext
    let component = maybe (ComponentName "UNKNOWN") id (lcComponent ctx)
    liftIO $ runFileLoggerWithComponent component $
      logAtLevel level msg metadata

runAppM :: AppEnv -> AppM a -> IO a
runAppM env (AppM r) = runReaderT r env

-- New: Run AppM with enhanced logging
runAppMWithLogging :: ComponentName -> AppEnv -> AppM a -> IO a
runAppMWithLogging component env action = do
  let enhancedEnv = env { aeLogContext = LogContext Nothing (Just component) Nothing }
  runAppM enhancedEnv action

-- DataProvider instance via CsvDataProvider adapter - now with logging
instance DataProvider AppM where
  loadTicks instr dr = do
    logInfo $ "Loading ticks for " <> showT instr <> " from " <> showT dr
    csv <- asks aeCsvProvider
    result <- liftIO $ runReaderT (runCsvDataProvider $ loadTicks instr dr) csv
    case result of
      Left err -> do
        logError $ "Failed to load ticks: " <> showT err
        return $ Left err  -- No conversion needed - already TempehError
      Right ticks -> do
        logInfo $ "Successfully loaded " <> showT (length ticks) <> " ticks"
        return $ Right ticks

  loadCandles instr dr period = do
    logInfo $ "Loading candles for " <> showT instr <> " period " <> showT period
    csv <- asks aeCsvProvider
    result <- liftIO $ runReaderT (runCsvDataProvider $ loadCandles instr dr period) csv
    case result of
      Left err -> do
        logError $ "Failed to load candles: " <> showT err
        return $ Left err  -- No conversion needed - already TempehError
      Right candles -> do
        logInfo $ "Successfully loaded " <> showT (length candles) <> " candles"
        return $ Right candles

  validateDataQuality ticks = do
    logInfo $ "Validating data quality for " <> showT (length ticks) <> " ticks"
    csv <- asks aeCsvProvider
    result <- liftIO $ runReaderT (runCsvDataProvider $ validateDataQuality ticks) csv
    case result of
      Left err -> do
        logError $ "Data quality validation failed: " <> showT err
        return $ Left err  -- No conversion needed - already TempehError
      Right quality -> do
        logInfo $ "Data quality score: " <> showT (dqrQualityScore quality)
        return $ Right quality

-- RiskManager instance via BasicRiskManager adapter - now with logging
instance RiskManager AppM where
  validateRiskLimits rl = do
    logInfo $ "Validating risk limits: " <> showT rl
    rm <- asks aeRiskManager
    result <- liftIO $ runReaderT (runBasicRiskManager $ validateRiskLimits rl) rm
    case result of
      Left err -> do
        logError $ "Risk limit validation failed: " <> showT err
        return $ Left err  -- No conversion needed - already TempehError
      Right () -> do
        logInfo "Risk limits validated successfully"
        return $ Right ()

  checkRiskLimits res rl = do
    logDebug $ "Checking risk limits for backtest result"
    rm <- asks aeRiskManager
    result <- liftIO $ runReaderT (runBasicRiskManager $ checkRiskLimits res rl) rm
    case result of
      Left err -> do
        logWarn $ "Risk limits exceeded: " <> showT err
        return $ Left err  -- No conversion needed - already TempehError
      Right () -> do
        logDebug "Risk limits check passed"
        return $ Right ()

  calculateDrawdown eq = do
    rm <- asks aeRiskManager
    liftIO $ runReaderT (runBasicRiskManager $ calculateDrawdown eq) rm
  validatePositionSize pos maxSz = do
    rm <- asks aeRiskManager
    liftIO $ runReaderT (runBasicRiskManager $ validatePositionSize pos maxSz) rm
  assessPortfolioRisk positions acct = do
    rm <- asks aeRiskManager
    liftIO $ runReaderT (runBasicRiskManager $ assessPortfolioRisk positions acct) rm

-- ReportGenerator instance via ConsoleReportGenerator adapter - now with logging
instance ReportGenerator AppM where
  generateReport res metrics cfg = do
    logInfo "Generating performance report"
    rg <- asks aeReportGen
    result <- liftIO $ runReaderT (runConsoleReportGenerator $ generateReport res metrics cfg) rg
    case result of
      Left err -> do
        logError $ "Report generation failed: " <> showT err
        return $ Left err  -- No conversion needed - already TempehError
      Right report -> do
        logInfo "Performance report generated successfully"
        return $ Right report

  generateTradeReport trades = do
    rg <- asks aeReportGen
    liftIO $ runReaderT (runConsoleReportGenerator $ generateTradeReport trades) rg
  generatePerformanceChart ec = do
    rg <- asks aeReportGen
    liftIO $ runReaderT (runConsoleReportGenerator $ generatePerformanceChart ec) rg
  exportReport out fmt path = do
    rg <- asks aeReportGen
    liftIO $ runReaderT (runConsoleReportGenerator $ exportReport out fmt path) rg

-- BacktestService instance delegates to BacktestEngine - now with logging
instance BacktestService AppM where
  executeBacktest params initState sigGen candles = do
    logInfo $ "Executing backtest with " <> showT (length candles) <> " candles"
    result <- liftIO $ evalStateT (runBacktestEngine $ executeBacktest params initState sigGen candles) dummyState
    case result of
      Left err -> do
        logError $ "Backtest execution failed: " <> showT err
        return $ Left err  -- No conversion needed - already TempehError
      Right backtestResult -> do
        logInfo $ "Backtest completed - Final balance: $" <> showT (brFinalBalance backtestResult)
        return $ Right backtestResult

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

-- Helper functions
showT :: Show a => a -> Text
showT = T.pack . show

-- Convert legacy AppError to new TempehError system
convertLegacyError :: AppError -> TempehError
convertLegacyError (ValidationError msg) = configError msg
convertLegacyError (DataLoadError msg) = dataError msg
convertLegacyError (StrategyInitError msg) = strategyError msg
convertLegacyError (ConfigError msg) = configError msg
convertLegacyError (InternalError msg) = systemError msg
