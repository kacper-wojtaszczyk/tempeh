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
import Util.Error (Result, AppError(..))
import Util.Logger (LogLevel(..), logInfo, logError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Scientific (Scientific, fromFloatDigits)
import qualified Data.Text as T

-- Orchestrator configuration
data BacktestConfig = BacktestConfig
  { bcInstrument :: Instrument
  , bcDateRange :: DateRange
  , bcStrategyParams :: StrategyParameters
  , bcInitialBalance :: Scientific
  , bcPositionSize :: Scientific
  , bcRiskLimits :: RiskLimits
  } deriving (Show)

-- Main orchestration function
orchestrateBacktest :: ( MonadIO m
                      , DataProvider m
                      , BacktestService m
                      , ReportGenerator m
                      , RiskManager m
                      )
                    => BacktestConfig
                    -> m (Result BacktestResult)
orchestrateBacktest config = do
  liftIO $ logInfo "Starting backtest orchestration"

  -- Step 1: Validate configuration
  validateResult <- validateConfiguration config
  case validateResult of
    Left err -> do
      liftIO $ logError $ "Configuration validation failed: " <> T.pack (show err)
      pure $ Left err
    Right () -> do

      -- Step 2: Load and validate data
      liftIO $ logInfo $ "Loading data for " <> T.pack (show (bcInstrument config))
      dataResult <- loadAndValidateData config
      case dataResult of
        Left err -> do
          liftIO $ logError $ "Data loading failed: " <> T.pack (show err)
          pure $ Left err
        Right candles -> do

          -- Step 3: Execute backtest with risk management
          liftIO $ logInfo "Executing backtest with risk management"
          backtestResult <- executeBacktestWithRiskManagement config candles
          case backtestResult of
            Left err -> do
              liftIO $ logError $ "Backtest execution failed: " <> T.pack (show err)
              pure $ Left err
            Right result -> do

              -- Step 4: Generate comprehensive report
              liftIO $ logInfo "Generating performance report"
              reportResult <- generatePerformanceReport result config
              case reportResult of
                Left err -> do
                  liftIO $ logError $ "Report generation failed: " <> T.pack (show err)
                  pure $ Left err
                Right finalResult -> do
                  liftIO $ logInfo "Backtest orchestration completed successfully"
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
            then pure $ Left $ ValidationError "Data quality too low for reliable backtesting"
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

  -- Create strategy instance (this would come from a factory in real implementation)
  let strategyInstance = createStrategyInstance (bcStrategyParams config)

  -- Execute backtest
  backtestResult <- executeBacktest params strategyInstance candles
  case backtestResult of
    Left err -> pure $ Left err
    Right result -> do
      -- Apply risk management checks
      riskCheckResult <- checkRiskLimits result (bcRiskLimits config)
      case riskCheckResult of
        Left err -> pure $ Left err
        Right () -> pure $ Right result

-- Generate comprehensive report
generatePerformanceReport :: (MonadIO m, BacktestService m, ReportGenerator m)
                          => BacktestResult
                          -> BacktestConfig
                          -> m (Result BacktestResult)
generatePerformanceReport result config = do
  -- Calculate performance metrics
  metricsResult <- calculatePerformanceMetrics result (bcInitialBalance config)
  case metricsResult of
    Left err -> pure $ Left err
    Right metrics -> do
      -- Generate report (pass () as placeholder for BacktestConfigType)
      reportResult <- generateReport result metrics ()
      case reportResult of
        Left err -> pure $ Left err
        Right _ -> pure $ Right result

-- Helper functions
createStrategyInstance :: StrategyParameters -> StrategyInstance
createStrategyInstance params = StrategyInstance
  { siName = "Strategy Instance"
  , siDescription = T.pack $ show params
  , siParameters = params
  , siSignalGenerator = \_ state -> (Hold, state)  -- Placeholder - would be implemented properly
  }

defaultBacktestConfig :: BacktestConfig
defaultBacktestConfig = BacktestConfig
  { bcInstrument = Instrument "EURUSD"
  , bcDateRange = DateRange 2025 1 2025 3
  , bcStrategyParams = EmaCrossParams 5 20 0.0001
  , bcInitialBalance = fromFloatDigits 10000.0
  , bcPositionSize = fromFloatDigits 1000.0
  , bcRiskLimits = RiskLimits
      { rlMaxDrawdown = fromFloatDigits 1000.0
      , rlMaxPositionSize = fromFloatDigits 5000.0
      , rlStopLossThreshold = fromFloatDigits 100.0
      }
  }
