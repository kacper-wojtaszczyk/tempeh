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
import Util.Logger (logInfo, logError, logInfoWithContext, MonadLogger)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Scientific (Scientific, fromFloatDigits)
import qualified Data.Text as T
import qualified Data.Aeson as A
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
  -- Create context for logging with dynamic values (now using proper JSON serialization)
  let backtestContext = A.object
        [ "instrument" A..= bcInstrument config  -- Now uses ToJSON instance
        , "dateRange" A..= bcDateRange config    -- Now uses ToJSON instance
        , "strategy" A..= spStrategyType (bcStrategyParams config)
        , "initialBalance" A..= bcInitialBalance config
        ]

  logInfoWithContext "Starting backtest orchestration" backtestContext

  -- Step 1: Validate configuration
  validateResult <- validateConfiguration config
  case validateResult of
    Left err -> do
      logError $ "Configuration validation failed: " <> T.pack (show err)
      pure $ Left err
    Right () -> do

      -- Step 2: Load and validate data
      logInfoWithContext "Loading market data" (A.object ["instrument" A..= bcInstrument config])
      dataResult <- loadAndValidateData config
      case dataResult of
        Left err -> do
          logError $ "Data loading failed: " <> T.pack (show err)
          pure $ Left err
        Right candles -> do

          -- Step 3: Execute backtest with risk management
          logInfoWithContext "Executing backtest with risk management"
            (A.object ["candleCount" A..= length candles, "strategy" A..= spStrategyType (bcStrategyParams config)])
          backtestResult <- executeBacktestWithRiskManagement config candles
          case backtestResult of
            Left err -> do
              logError $ "Backtest execution failed: " <> T.pack (show err)
              pure $ Left err
            Right result -> do

              -- Step 4: Generate comprehensive report
              logInfoWithContext "Generating performance report"
                (A.object ["finalBalance" A..= brFinalBalance result, "totalTrades" A..= brTotalTrades result])
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
validateConfiguration :: (MonadIO m, RiskManager m, BacktestService m, MonadLogger m) => BacktestConfig -> m (Result ())
validateConfiguration config = do
  let validationContext = A.object
        [ "initialBalance" A..= bcInitialBalance config
        , "positionSize" A..= bcPositionSize config
        , "instrument" A..= bcInstrument config
        , "maxDrawdown" A..= rlMaxDrawdown (bcRiskLimits config)
        ]

  logInfoWithContext "Validating backtest configuration" validationContext

  let params = BacktestParameters
        { bpInitialBalance = bcInitialBalance config
        , bpPositionSize = bcPositionSize config
        , bpInstrument = bcInstrument config
        , bpCommission = fromFloatDigits 0.0001
        , bpSlippage = fromFloatDigits 0.0001
        }

  paramValidation <- validateBacktestParameters params
  case paramValidation of
    Left err -> do
      logError $ "Parameter validation failed: " <> T.pack (show err)
      pure $ Left err
    Right () -> do
      logInfoWithContext "Validating risk limits" validationContext
      riskResult <- validateRiskLimits (bcRiskLimits config)
      case riskResult of
        Left err -> logError $ "Risk limits validation failed: " <> T.pack (show err)
        Right () -> logInfo "Configuration validation completed successfully"
      pure riskResult

-- Data loading and validation
loadAndValidateData :: (MonadIO m, DataProvider m, MonadLogger m) => BacktestConfig -> m (Result [Candle])
loadAndValidateData config = do
  let dataContext = A.object
        [ "instrument" A..= bcInstrument config
        , "dateRange" A..= bcDateRange config
        ]

  logInfoWithContext "Loading tick data from data provider" dataContext

  -- Load tick data
  ticksResult <- loadTicks (bcInstrument config) (bcDateRange config)
  case ticksResult of
    Left err -> do
      logError $ "Tick data loading failed: " <> T.pack (show err)
      pure $ Left err
    Right ticks -> do
      let tickContext = A.object
            [ "tickCount" A..= length ticks
            , "instrument" A..= bcInstrument config
            ]
      logInfoWithContext "Validating data quality" tickContext

      -- Validate data quality
      qualityResult <- validateDataQuality ticks
      case qualityResult of
        Left err -> do
          logError $ "Data quality validation failed: " <> T.pack (show err)
          pure $ Left err
        Right qualityReport -> do
          let qualityContext = A.object
                [ "qualityScore" A..= dqrQualityScore qualityReport
                , "tickCount" A..= length ticks
                , "threshold" A..= (50 :: Int)
                ]

          logInfoWithContext "Data quality assessment complete" qualityContext

          if dqrQualityScore qualityReport < 50  -- Minimum quality threshold
            then do
              logError "Data quality below minimum threshold for reliable backtesting"
              pure $ Left $ dataError "Data quality too low for reliable backtesting"
            else do
              logInfoWithContext "Converting ticks to candles" (A.object ["period" A..= ("OneMinute" :: T.Text)])
              -- Convert to candles
              candlesResult <- loadCandles (bcInstrument config) (bcDateRange config) OneMinute
              case candlesResult of
                Left err -> logError $ "Candle conversion failed: " <> T.pack (show err)
                Right candles -> logInfoWithContext "Candles loaded successfully"
                  (A.object ["candleCount" A..= length candles])
              pure candlesResult

-- Backtest execution with risk management
executeBacktestWithRiskManagement :: ( MonadIO m
                                     , BacktestService m
                                     , RiskManager m
                                     , MonadLogger m
                                     )
                                   => BacktestConfig
                                   -> [Candle]
                                   -> m (Result BacktestResult)
executeBacktestWithRiskManagement config candles = do
  let executionContext = A.object
        [ "candleCount" A..= length candles
        , "strategy" A..= spStrategyType (bcStrategyParams config)
        , "initialBalance" A..= bcInitialBalance config
        , "positionSize" A..= bcPositionSize config
        ]

  logInfoWithContext "Initializing backtest execution" executionContext

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
    Left err -> do
      logError $ "Strategy creation failed: " <> T.pack (show err)
      pure (Left err)
    Right strategyInstance -> do
      logInfoWithContext "Strategy instance created"
        (A.object ["strategyName" A..= siName strategyInstance])

      -- Execute backtest with initial state and signal generator
      backtestResult <- executeBacktest params (siInitialState strategyInstance) (siSignalGenerator strategyInstance) candles
      case backtestResult of
        Left err -> do
          logError $ "Backtest execution failed: " <> T.pack (show err)
          pure $ Left err
        Right result -> do
          let resultContext = A.object
                [ "finalBalance" A..= brFinalBalance result
                , "totalTrades" A..= brTotalTrades result
                , "pnl" A..= brPnL result
                ]
          logInfoWithContext "Backtest execution completed, applying risk checks" resultContext

          -- Apply risk management checks
          riskCheckResult <- checkRiskLimits result (bcRiskLimits config)
          case riskCheckResult of
            Left err -> do
              logError $ "Risk limit checks failed: " <> T.pack (show err)
              pure $ Left err
            Right () -> do
              logInfo "Risk limit checks passed"
              pure $ Right result

-- Helper function to create strategy instance from parameters using registry
createStrategyInstanceFromParams :: StrategyRegistry -> StrategyParameters -> Result StrategyInstance
createStrategyInstanceFromParams registry params =
  let strategyType = spStrategyType params
  in case findStrategyByKeyword (T.unpack strategyType) registry of
       Just provider -> Right (spFactory provider params)
       Nothing -> Left $ configError $ "Unknown strategy type: " <> strategyType

-- Generate comprehensive report
generatePerformanceReport :: (MonadIO m, BacktestService m, ReportGenerator m, MonadLogger m)
                          => BacktestResult
                          -> BacktestConfig
                          -> ReportContext
                          -> m (Result BacktestResult)
generatePerformanceReport result config rptCtx = do
  let reportContext = A.object
        [ "finalBalance" A..= brFinalBalance result
        , "totalTrades" A..= brTotalTrades result
        , "instrument" A..= bcInstrument config
        ]

  logInfoWithContext "Calculating performance metrics" reportContext

  -- Calculate performance metrics
  metricsResult <- calculatePerformanceMetrics result (bcInitialBalance config)
  case metricsResult of
    Left err -> do
      logError $ "Performance metrics calculation failed: " <> T.pack (show err)
      pure $ Left err
    Right metrics -> do
      let metricsContext = A.object
            [ "winRate" A..= pmWinRate metrics
            , "profitFactor" A..= pmProfitFactor metrics
            , "maxDrawdown" A..= pmMaxDrawdown metrics
            ]
      logInfoWithContext "Performance metrics calculated" metricsContext

      -- Generate report with context
      reportResult <- generateReport result metrics rptCtx
      case reportResult of
        Left err -> do
          logError $ "Report generation failed: " <> T.pack (show err)
          pure $ Left err
        Right _ -> do
          logInfo "Performance report generated successfully"
          pure $ Right result

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
