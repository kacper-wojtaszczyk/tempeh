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
import qualified Data.Aeson as A

import Domain.Services.BacktestService
import Domain.Types
import Domain.Strategy (StrategyState(..))

import Application.ReportingService (ReportGenerator(..))
import Adapter.CsvDataProvider
import Adapter.RiskManagerAdapter
import Adapter.ReportGeneratorAdapter
import Adapter.BacktestEngine

-- Import our new logging and error handling
import Util.Logger (MonadLogger(..), LogContext(..), ComponentName(..), CorrelationId(..), runFileLoggerWithComponent, emptyLogContext, logInfo, logError, logWarn, logDebug, logInfoWithContext, logErrorWithContext, logWarnWithContext, logDebugWithContext)
import Util.Error (TempehError, Result, AppError(..), dataError, strategyError, configError, systemError)

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
    let tickContext = A.object
          [ "instrument" A..= instr
          , "dateRange" A..= dr
          , "dataSource" A..= ("CSV" :: T.Text)
          ]
    logInfoWithContext "Loading ticks from data provider" tickContext
    csv <- asks aeCsvProvider
    result <- liftIO $ runReaderT (runCsvDataProvider $ loadTicks instr dr) csv
    case result of
      Left err -> do
        logErrorWithContext "Failed to load ticks"
          (A.object ["error" A..= show err, "instrument" A..= instr])
        return $ Left err  -- No conversion needed - already TempehError
      Right ticks -> do
        logInfoWithContext "Successfully loaded ticks"
          (A.object ["tickCount" A..= length ticks, "instrument" A..= instr])
        return $ Right ticks

  loadCandles instr dr period = do
    let candleContext = A.object
          [ "instrument" A..= instr
          , "dateRange" A..= dr
          , "period" A..= show period
          , "dataSource" A..= ("CSV" :: T.Text)
          ]
    logInfoWithContext "Loading candles from data provider" candleContext
    csv <- asks aeCsvProvider
    result <- liftIO $ runReaderT (runCsvDataProvider $ loadCandles instr dr period) csv
    case result of
      Left err -> do
        logErrorWithContext "Failed to load candles"
          (A.object ["error" A..= show err, "instrument" A..= instr, "period" A..= show period])
        return $ Left err  -- No conversion needed - already TempehError
      Right candles -> do
        logInfoWithContext "Successfully loaded candles"
          (A.object ["candleCount" A..= length candles, "instrument" A..= instr])
        return $ Right candles

  validateDataQuality ticks = do
    let qualityContext = A.object
          [ "tickCount" A..= length ticks
          , "validationType" A..= ("quality" :: T.Text)
          ]
    logInfoWithContext "Validating data quality" qualityContext
    csv <- asks aeCsvProvider
    result <- liftIO $ runReaderT (runCsvDataProvider $ validateDataQuality ticks) csv
    case result of
      Left err -> do
        logErrorWithContext "Data quality validation failed"
          (A.object ["error" A..= show err, "tickCount" A..= length ticks])
        return $ Left err  -- No conversion needed - already TempehError
      Right quality -> do
        logInfoWithContext "Data quality validation complete"
          (A.object ["qualityScore" A..= dqrQualityScore quality, "tickCount" A..= length ticks])
        return $ Right quality

-- RiskManager instance via BasicRiskManager adapter - now with logging
instance RiskManager AppM where
  validateRiskLimits rl = do
    let riskContext = A.object
          [ "maxDrawdown" A..= rlMaxDrawdown rl
          , "maxPositionSize" A..= rlMaxPositionSize rl
          , "stopLossThreshold" A..= rlStopLossThreshold rl
          ]
    logInfoWithContext "Validating risk limits" riskContext
    rm <- asks aeRiskManager
    result <- liftIO $ runReaderT (runBasicRiskManager $ validateRiskLimits rl) rm
    case result of
      Left err -> do
        logErrorWithContext "Risk limit validation failed"
          (A.object ["error" A..= show err])
        return $ Left err  -- No conversion needed - already TempehError
      Right () -> do
        logInfo "Risk limits validated successfully"
        return $ Right ()

  checkRiskLimits res rl = do
    let checkContext = A.object
          [ "finalBalance" A..= brFinalBalance res
          , "pnl" A..= brPnL res
          , "maxDrawdown" A..= rlMaxDrawdown rl
          , "totalTrades" A..= brTotalTrades res
          ]
    logDebugWithContext "Checking risk limits for backtest result" checkContext
    rm <- asks aeRiskManager
    result <- liftIO $ runReaderT (runBasicRiskManager $ checkRiskLimits res rl) rm
    case result of
      Left err -> do
        logWarnWithContext "Risk limits exceeded"
          (A.object ["error" A..= show err, "finalBalance" A..= brFinalBalance res])
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
    let reportContext = A.object
          [ "finalBalance" A..= brFinalBalance res
          , "totalTrades" A..= brTotalTrades res
          , "winRate" A..= pmWinRate metrics
          , "profitFactor" A..= pmProfitFactor metrics
          ]
    logInfoWithContext "Generating performance report" reportContext
    rg <- asks aeReportGen
    result <- liftIO $ runReaderT (runConsoleReportGenerator $ generateReport res metrics cfg) rg
    case result of
      Left err -> do
        logErrorWithContext "Report generation failed"
          (A.object ["error" A..= show err])
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
    let backtestContext = A.object
          [ "candleCount" A..= length candles
          , "initialBalance" A..= bpInitialBalance params
          , "positionSize" A..= bpPositionSize params
          , "instrument" A..= bpInstrument params
          ]
    logInfoWithContext "Executing backtest" backtestContext
    result <- liftIO $ evalStateT (runBacktestEngine $ executeBacktest params initState sigGen candles) dummyState
    case result of
      Left err -> do
        logErrorWithContext "Backtest execution failed"
          (A.object ["error" A..= show err, "candleCount" A..= length candles])
        return $ Left err  -- No conversion needed - already TempehError
      Right backtestResult -> do
        logInfoWithContext "Backtest completed successfully"
          (A.object
            [ "finalBalance" A..= brFinalBalance backtestResult
            , "totalTrades" A..= brTotalTrades backtestResult
            , "pnl" A..= brPnL backtestResult
            ])
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
