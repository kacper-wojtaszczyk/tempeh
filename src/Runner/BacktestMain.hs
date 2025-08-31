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
import Strategy.EmaCross (emaCrossStrategy)
import Util.Logger (Logger, mkLogger, logWithLevel)
import Util.Error (Result, AppError(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T

-- Configuration for backtest execution
data BacktestOptions = BacktestOptions
  { boInstrument :: Instrument
  , boDataFilter :: DataFilter
  , boDescription :: String
  , boConfig :: Maybe AppConfig  -- Optional override
  } deriving (Show)

-- Main backtest application runner
runBacktestApp :: BacktestOptions -> IO ()
runBacktestApp options = do
  let config = maybe defaultAppConfig id (boConfig options)
      logger = mkLogger (acLogging config)

  logWithLevel logger (acLogging config) "Info" $ "=== TEMPEH BACKTEST ==="
  logWithLevel logger (acLogging config) "Info" $ T.pack $ boDescription options

  -- Load and process data
  result <- runBacktestPipeline config logger options
  case result of
    Left err -> do
      logWithLevel logger (acLogging config) "Error" $ "Backtest failed: " <> T.pack (show err)
      error $ "Backtest execution failed: " <> show err
    Right report -> printReport report

-- Pure backtest pipeline - separates IO from computation
runBacktestPipeline :: AppConfig -> Logger -> BacktestOptions -> IO (Result BacktestReport)
runBacktestPipeline config logger options = do
  -- Load ticks
  ticksResult <- loadTicksWithConfig config logger (boInstrument options) (boDataFilter options)
  case ticksResult of
    Left err -> pure $ Left err
    Right ticks -> do
      logWithLevel logger (acLogging config) "Info" $ "Loaded " <> T.pack (show (length ticks)) <> " ticks"

      -- Convert to candles (pure operation)
      let candles = aggregateTicksToCandles ticks
      logWithLevel logger (acLogging config) "Info" $ "Generated " <> T.pack (show (length candles)) <> " candles"

      -- Run backtest (pure operation)
      pure $ runPureBacktest config options candles

-- Pure backtest execution - no IO
runPureBacktest :: AppConfig -> BacktestOptions -> [Candle] -> Result BacktestReport
runPureBacktest config options candles
  | null candles = Left $ BacktestError "No candles available for backtesting"
  | otherwise =
    let strategy = emaCrossStrategy 5 20  -- Could be parameterized
        backtestConfig = Engine.BacktestConfig
          { Engine.bcInitialBalance = bcInitialBalance (acBacktest config)
          , Engine.bcPositionSize = bcPositionSize (acBacktest config)
          , Engine.bcInstrument = boInstrument options
          }
        result = Engine.runBacktest backtestConfig strategy candles
        report = generateReport (Engine.bcInitialBalance backtestConfig) result
    in Right report
