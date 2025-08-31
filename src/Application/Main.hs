{-# LANGUAGE OverloadedStrings #-}
module Application.Main where

import Adapter.CsvDataProvider
import Adapter.BacktestEngine
import Adapter.StrategyFactory
import Domain.Services.BacktestService
import Domain.Types
import Port.DataProvider
import Util.Error (Result)
import Util.Config (defaultAppConfig, acDataDirectory)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Scientific (fromFloatDigits)
import qualified Data.Text as T

-- Main application entry point using new architecture
runBacktestWithNewArchitecture :: Instrument -> DateRange -> StrategyParameters -> IO ()
runBacktestWithNewArchitecture instrument dateRange strategyParams = do
  putStrLn "=== TEMPEH BACKTEST (NEW ARCHITECTURE) ==="
  putStrLn $ "Instrument: " <> show instrument
  putStrLn $ "Date Range: " <> show dateRange
  putStrLn $ "Strategy: " <> show strategyParams
  putStrLn ""

  let config = defaultAppConfig
      csvProvider = CsvDataProvider (acDataDirectory config)
      strategyInstance = createStrategyFromConfig strategyParams
      params = defaultBacktestParams instrument

  -- Step 1: Load data using CSV adapter
  putStrLn "Loading data..."
  ticksResult <- runReaderT (runCsvDataProvider $ loadTicks instrument dateRange) csvProvider

  case ticksResult of
    Left err -> putStrLn $ "Data loading failed: " <> show err
    Right ticks -> do
      putStrLn $ "Loaded " <> show (length ticks) <> " ticks"

      -- Step 2: Convert to candles
      putStrLn "Converting to candles..."
      candlesResult <- runReaderT (runCsvDataProvider $ loadCandles instrument dateRange OneMinute) csvProvider

      case candlesResult of
        Left err -> putStrLn $ "Candle conversion failed: " <> show err
        Right candles -> do
          putStrLn $ "Generated " <> show (length candles) <> " candles"

          -- Step 3: Run backtest using engine adapter
          putStrLn "Executing backtest..."
          let initialEngineState = BacktestEngineState
                { besBalance = bpInitialBalance params
                , besPosition = Nothing
                , besTrades = []
                , besStrategyState = StrategyState ""
                , besEquityCurve = []
                }

          (backtestResult, _) <- runStateT (runBacktestEngine $ executeBacktest params strategyInstance candles) initialEngineState

          case backtestResult of
            Left err -> putStrLn $ "Backtest execution failed: " <> show err
            Right result -> do
              putStrLn "Backtest completed successfully!"

              -- Step 4: Calculate metrics
              (metricsResult, _) <- runStateT (runBacktestEngine $ calculatePerformanceMetrics result (bpInitialBalance params)) initialEngineState

              case metricsResult of
                Left err -> putStrLn $ "Metrics calculation failed: " <> show err
                Right metrics -> printResults result metrics

-- Helper functions
defaultBacktestParams :: Instrument -> BacktestParameters
defaultBacktestParams instrument = BacktestParameters
  { bpInitialBalance = fromFloatDigits 10000.0
  , bpPositionSize = fromFloatDigits 1000.0
  , bpInstrument = instrument
  , bpCommission = fromFloatDigits 0.0001
  , bpSlippage = fromFloatDigits 0.0001
  }

printResults :: BacktestResult -> PerformanceMetrics -> IO ()
printResults result metrics = do
  putStrLn "\n=== BACKTEST RESULTS ==="
  putStrLn $ "Final Balance: $" <> show (brFinalBalance result)
  putStrLn $ "Total P&L: $" <> show (brPnL result)
  putStrLn $ "Total Trades: " <> show (brTotalTrades result)
  putStrLn ""
  putStrLn "=== PERFORMANCE METRICS ==="
  putStrLn $ "Win Rate: " <> show (pmWinRate metrics) <> "%"
  putStrLn $ "Profit Factor: " <> show (pmProfitFactor metrics)
  putStrLn $ "Average Win: $" <> show (pmAverageWin metrics)
  putStrLn $ "Average Loss: $" <> show (pmAverageLoss metrics)
  putStrLn $ "Max Drawdown: $" <> show (pmMaxDrawdown metrics)
