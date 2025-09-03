{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Application.BacktestOrchestrator
  ( orchestrateBacktest
  , BacktestConfig(..)
  , defaultBacktestConfig
  ) where

import Domain.Types
import Domain.Services.BacktestService
import Application.ReportingService
import Application.Strategy.Types (StrategyParameters(..), StrategyInstance(..), StrategyProvider(..))
import Application.Strategy.Factory (initializeStrategyRegistry)
import Application.Strategy.Registry (StrategyRegistry, findStrategyByKeyword)
import Util.Error (Result, AppError(..), dataError, configError)
import Util.Logger (logInfo, logError, MonadLogger)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Scientific (Scientific, fromFloatDigits)
import qualified Data.Text as T
import qualified Strategy.EmaCross as EmaCross

-- Orchestrator configuration
data BacktestConfig = BacktestConfig
  { bcInstrument :: Instrument
  , bcDateRange :: DateRange
  , bcStrategyParams :: StrategyParameters
  , bcInitialBalance :: Scientific
  , bcPositionSize :: Scientific
  , bcRiskLimits :: RiskLimits
  } deriving (Show)

-- Main orchestration function with proper logging constraint
orchestrateBacktest :: ( MonadIO m
                      , DataProvider m
                      , BacktestService m
                      , ReportGenerator m
                      , RiskManager m
                      , MonadLogger m
                      )
                    => BacktestConfig
                    -> m (Result BacktestResult)
orchestrateBacktest config = do
  logInfo "Starting backtest orchestration"

  -- Step 1: Validate configuration
  validateResult <- validateConfiguration config
  case validateResult of
    Left err -> do
      logError $ "Configuration validation failed: " <> T.pack (show err)
      pure $ Left err
    Right () -> do

      -- Step 2: Load and validate data
      logInfo $ "Loading data for " <> T.pack (show (bcInstrument config))
      dataResult <- loadAndValidateData config
      case dataResult of
        Left err -> do
          logError $ "Data loading failed: " <> T.pack (show err)
          pure $ Left err
        Right candles -> do

          -- Step 3: Execute backtest with risk management
          logInfo "Executing backtest with risk management"
          backtestResult <- executeBacktestWithRiskManagement config candles
          case backtestResult of
            Left err -> do
              logError $ "Backtest execution failed: " <> T.pack (show err)
              pure $ Left err
            Right result -> do

              -- Step 4: Generate comprehensive report
              logInfo "Generating performance report"
              let rptCtx = ReportContext (bcInstrument config) (bcDateRange config) (bcStrategyParams config) (bcInitialBalance config)
              reportResult <- generatePerformanceReport result config rptCtx
              case reportResult of
                Left err -> do
                  logError $ "Report generation failed: " <> T.pack (show err)
                  pure $ Left err
                Right finalResult -> do
                  logInfo "Backtest orchestration completed successfully"
                  pure $ Right finalResult

-- Configuration validation
validateConfiguration :: (MonadIO m, RiskManager m, BacktestService m) => BacktestConfig -> m (Result ())
validateConfiguration config = do
  let params = BacktestParameters
        { bpInitialBalance = bcInitialBalance config
        , bpPositionSize = bcPositionSize config
        , bpInstrument = bcInstrument config
        , bpCommission = fromFloatDigits 0.0001
        , bpSlippage = fromFloatDigits 0.0001
        }

  paramValidation <- validateBacktestParameters params
  case paramValidation of
    Left err -> pure $ Left err
    Right () -> validateRiskLimits (bcRiskLimits config)

-- Data loading and validation
loadAndValidateData :: (MonadIO m, DataProvider m) => BacktestConfig -> m (Result [Candle])
loadAndValidateData config = do
  -- Load tick data
  ticksResult <- loadTicks (bcInstrument config) (bcDateRange config)
  case ticksResult of
    Left err -> pure $ Left err
    Right ticks -> do
      -- Validate data quality
      qualityResult <- validateDataQuality ticks
      case qualityResult of
        Left err -> pure $ Left err
        Right qualityReport -> do
          if dqrQualityScore qualityReport < 50  -- Minimum quality threshold
            then pure $ Left $ dataError "Data quality too low for reliable backtesting"
            else do
              -- Convert to candles
              candlesResult <- loadCandles (bcInstrument config) (bcDateRange config) OneMinute
              pure candlesResult

-- Backtest execution with risk management
executeBacktestWithRiskManagement :: ( MonadIO m
                                     , BacktestService m
                                     , RiskManager m
                                     )
                                   => BacktestConfig
                                   -> [Candle]
                                   -> m (Result BacktestResult)
executeBacktestWithRiskManagement config candles = do
  let params = BacktestParameters
        { bpInitialBalance = bcInitialBalance config
        , bpPositionSize = bcPositionSize config
        , bpInstrument = bcInstrument config
        , bpCommission = fromFloatDigits 0.0001
        , bpSlippage = fromFloatDigits 0.0001
        }

  -- Create strategy instance using the registry system
  let registry = initializeStrategyRegistry
  case createStrategyInstanceFromParams registry (bcStrategyParams config) of
    Left err -> pure (Left err)
    Right strategyInstance -> do
      -- Execute backtest with initial state and signal generator
      backtestResult <- executeBacktest params (siInitialState strategyInstance) (siSignalGenerator strategyInstance) candles
      case backtestResult of
        Left err -> pure $ Left err
        Right result -> do
          -- Apply risk management checks
          riskCheckResult <- checkRiskLimits result (bcRiskLimits config)
          case riskCheckResult of
            Left err -> pure $ Left err
            Right () -> pure $ Right result

-- Helper function to create strategy instance from parameters using registry
createStrategyInstanceFromParams :: StrategyRegistry -> StrategyParameters -> Result StrategyInstance
createStrategyInstanceFromParams registry params =
  let strategyType = spStrategyType params
  in case findStrategyByKeyword (T.unpack strategyType) registry of
       Just provider -> Right (spFactory provider params)
       Nothing -> Left $ configError $ "Unknown strategy type: " <> strategyType

-- Generate comprehensive report
generatePerformanceReport :: (MonadIO m, BacktestService m, ReportGenerator m)
                          => BacktestResult
                          -> BacktestConfig
                          -> ReportContext
                          -> m (Result BacktestResult)
generatePerformanceReport result config rptCtx = do
  -- Calculate performance metrics
  metricsResult <- calculatePerformanceMetrics result (bcInitialBalance config)
  case metricsResult of
    Left err -> pure $ Left err
    Right metrics -> do
      -- Generate report with context
      reportResult <- generateReport result metrics rptCtx
      case reportResult of
        Left err -> pure $ Left err
        Right _ -> pure $ Right result

-- Obtain default EMA parameters from the registry in a pure way
getDefaultEmaParams :: StrategyParameters
getDefaultEmaParams =
  let registry = initializeStrategyRegistry
  in case findStrategyByKeyword "ema" registry of
       Just provider -> spDefaultParams provider
       -- Safe fallback to EMA provider defaults if registry lookup fails
       Nothing -> spDefaultParams EmaCross.strategyProvider

defaultBacktestConfig :: BacktestConfig
defaultBacktestConfig = BacktestConfig
  { bcInstrument = Instrument "EURUSD"
  , bcDateRange = DateRange 2025 1 2025 3
  , bcStrategyParams = getDefaultEmaParams
  , bcInitialBalance = fromFloatDigits 10000.0
  , bcPositionSize = fromFloatDigits 1000.0
  , bcRiskLimits = RiskLimits
      { rlMaxDrawdown = fromFloatDigits 1000.0
      , rlMaxPositionSize = fromFloatDigits 5000.0
      , rlStopLossThreshold = fromFloatDigits 100.0
      }
  }
