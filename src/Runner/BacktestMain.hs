{-# LANGUAGE OverloadedStrings #-}
module Runner.BacktestMain
  ( runBacktestApp
  , BacktestOptions(..)
  ) where

import qualified Backtest.Engine as Engine
import Backtest.Report (BacktestReport, generateReport, printReport)
import Backtest.CsvTickLoader (DataFilter(..), DateRange(..), loadTicksWithConfig)
import Backtest.CandleAggregator (aggregateTicksToCandles)
import Domain.Types (Instrument(..), Candle)
import Util.Config (AppConfig(..), defaultAppConfig, BacktestConfig(..))
import Strategy.EmaCross (emaCrossStrategyWithConfig)
import Strategy.Config (StrategyConfig(..), StrategyParameters(..), defaultEmaCrossConfig)
import Util.Logger (Logger, mkLogger, logWithLevel)
import Util.Error (Result, AppError(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import System.IO (hFlush, stdout)

-- Configuration for backtest execution
data BacktestOptions = BacktestOptions
  { boInstrument :: Instrument
  , boDataFilter :: DataFilter
  , boDescription :: String
  , boConfig :: Maybe AppConfig  -- Optional override
  , boStrategyConfig :: StrategyConfig -- Strategy configuration
  } deriving (Show)

-- Main backtest application runner with immediate console output
runBacktestApp :: BacktestOptions -> IO ()
runBacktestApp options = do
  putStrLn "=== TEMPEH BACKTEST DEBUG ==="
  putStrLn $ "Running: " <> boDescription options
  putStrLn $ "Instrument: " <> show (boInstrument options)
  putStrLn $ "Filter: " <> show (boDataFilter options)
  hFlush stdout

  let config = maybe defaultAppConfig id (boConfig options)
      logger = mkLogger (acLogging config)

  -- Load and process data with debugging
  result <- runBacktestPipeline config logger options
  case result of
    Left err -> do
      putStrLn $ "BACKTEST FAILED: " <> show err
      error $ "Backtest execution failed: " <> show err
    Right report -> do
      putStrLn "Backtest completed successfully!"
      printReport report

-- Pure backtest pipeline with extensive debugging
runBacktestPipeline :: AppConfig -> Logger -> BacktestOptions -> IO (Result BacktestReport)
runBacktestPipeline config logger options = do
  putStrLn "=== DATA LOADING PHASE ==="
  putStrLn $ "Data directory: " <> acDataDirectory config
  putStrLn $ "Loading ticks for: " <> show (boInstrument options)
  putStrLn $ "Date filter: " <> show (boDataFilter options)
  hFlush stdout

  -- Load ticks with debugging
  ticksResult <- loadTicksWithConfig config logger (boInstrument options) (boDataFilter options)
  case ticksResult of
    Left err -> do
      putStrLn $ "ERROR loading ticks: " <> show err
      pure $ Left err
    Right ticks -> do
      putStrLn $ "SUCCESS: Loaded " <> show (length ticks) <> " ticks"
      hFlush stdout

      -- Convert to candles with debugging
      putStrLn "=== CANDLE AGGREGATION PHASE ==="
      let candles = aggregateTicksToCandles ticks
      putStrLn $ "Generated " <> show (length candles) <> " candles"

      -- Show sample data for debugging
      putStrLn "Sample ticks (first 3):"
      mapM_ print (take 3 ticks)
      putStrLn "Sample candles (first 3):"
      mapM_ print (take 3 candles)
      hFlush stdout

      -- Run backtest with debugging
      putStrLn "=== BACKTEST EXECUTION PHASE ==="
      pure $ runPureBacktest config options candles

-- Pure backtest execution - now with configurable strategy
runPureBacktest :: AppConfig -> BacktestOptions -> [Candle] -> Result BacktestReport
runPureBacktest config options candles
  | null candles = Left $ BacktestError "No candles available for backtesting"
  | otherwise =
    let strategy = case scParameters (boStrategyConfig options) of
          EmaCrossParams{} -> emaCrossStrategyWithConfig (scParameters (boStrategyConfig options))
          _ -> error "Unsupported strategy type"

        backtestConfig = Engine.BacktestConfig
          { Engine.bcInitialBalance = bcInitialBalance (acBacktest config)
          , Engine.bcPositionSize = bcPositionSize (acBacktest config)
          , Engine.bcInstrument = boInstrument options
          }
        result = Engine.runBacktest backtestConfig strategy candles
        report = generateReport (Engine.bcInitialBalance backtestConfig) result
    in Right report
