{-# LANGUAGE OverloadedStrings #-}
module Application.CLI
  ( runCLI
  , parseCommand
  , Command(..)
  ) where

import System.Environment (getArgs)
import Adapter.CsvDataProvider (CsvDataProvider(..))
import Adapter.RiskManagerAdapter (BasicRiskManager, defaultBasicRiskManager)
import Adapter.ReportGeneratorAdapter (ConsoleReportGenerator(..))
import Application.BacktestOrchestrator (orchestrateBacktest, BacktestConfig(..), defaultBacktestConfig)
import Application.Env (AppEnv(..), runAppM)
import Domain.Services.BacktestService ( StrategyParameters(..)
                                       , DateRange(..)
                                       , BacktestResult(..)
                                       )
import Domain.Types (Instrument(..))
import Util.Config (defaultAppConfig, acDataDirectory)
import Data.Scientific (fromFloatDigits)
import Text.Read (readMaybe)
import qualified Data.Text as T

-- CLI Commands
data Command
  = BacktestCommand Instrument DateRange StrategyParameters
  | HelpCommand
  | InvalidCommand String
  deriving (Show)

-- Main CLI entry point
runCLI :: IO ()
runCLI = do
  args <- getArgs
  case parseCommand args of
    BacktestCommand instrument dateRange strategyParams ->
      runBacktest instrument dateRange strategyParams
    HelpCommand ->
      printUsage
    InvalidCommand err -> do
      putStrLn $ "Error: " ++ err
      printUsage

-- Command parsing
parseCommand :: [String] -> Command
parseCommand [] = HelpCommand
parseCommand ["--help"] = HelpCommand
parseCommand ["-h"] = HelpCommand
parseCommand ["backtest", instrumentStr, startYearStr, startMonthStr, endYearStr, endMonthStr] =
  case (parseInstrument instrumentStr, parseTimeRange startYearStr startMonthStr endYearStr endMonthStr) of
    (Just instrument, Just dateRange) ->
      let strategyParams = EmaCrossParams 5 20 0.0001  -- Default EMA parameters
      in BacktestCommand instrument dateRange strategyParams
    (Nothing, _) -> InvalidCommand $ "Invalid instrument format: " ++ instrumentStr ++ ". Use alphanumeric characters only."
    (_, Nothing) -> InvalidCommand "Invalid date format. Use: backtest <instrument> <start_year> <start_month> <end_year> <end_month>"
parseCommand args = InvalidCommand $ "Invalid arguments: " ++ unwords args

-- Backtest execution
runBacktest :: Instrument -> DateRange -> StrategyParameters -> IO ()
runBacktest instrument dateRange strategyParams = do
  putStrLn "=== TEMPEH BACKTEST ==="
  putStrLn $ "Instrument: " <> show instrument
  putStrLn $ "Date Range: " <> show dateRange
  putStrLn $ "Strategy: " <> show strategyParams
  putStrLn ""

  let config = defaultAppConfig
      env = AppEnv
        { aeCsvProvider = CsvDataProvider (acDataDirectory config)
        , aeRiskManager = defaultBasicRiskManager
        , aeReportGen   = ConsoleReportGenerator
        }
      btConfig = defaultBacktestConfig
        { bcInstrument = instrument
        , bcDateRange = dateRange
        , bcStrategyParams = strategyParams
        , bcInitialBalance = fromFloatDigits 10000.0
        , bcPositionSize = fromFloatDigits 1000.0
        }

  result <- runAppM env (orchestrateBacktest btConfig)
  case result of
    Left err -> putStrLn $ "Backtest failed: " <> show err
    Right final -> do
      -- Orchestrator already generated report; print a succinct summary
      putStrLn "\n=== BACKTEST RESULTS ==="
      putStrLn $ "Final Balance: $" <> show (brFinalBalance final)
      putStrLn $ "Total P&L: $" <> show (brPnL final)
      putStrLn $ "Total Trades: " <> show (brTotalTrades final)

-- Help and usage
printUsage :: IO ()
printUsage = do
  putStrLn "TEMPEH Trading Bot - Ports & Adapters Architecture"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  tempeh backtest <instrument> <start_year> <start_month> <end_year> <end_month>"
  putStrLn "  tempeh --help"
  putStrLn ""
  putStrLn "Parameters:"
  putStrLn "  instrument     - Currency pair (e.g., EURUSD, GBPUSD, USDJPY)"
  putStrLn "  start_year     - Start year (e.g., 2025)"
  putStrLn "  start_month    - Start month (1-12)"
  putStrLn "  end_year       - End year (e.g., 2025)"
  putStrLn "  end_month      - End month (1-12)"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  tempeh backtest EURUSD 2025 1 2025 3      - Run EURUSD backtest from Jan 2025 to Mar 2025"
  putStrLn "  tempeh backtest GBPUSD 2025 7 2025 8      - Run GBPUSD backtest from Jul 2025 to Aug 2025"
  putStrLn "  tempeh backtest USDJPY 2025 6 2025 6      - Run USDJPY backtest for June 2025 only"

-- Parsing helpers
parseInstrument :: String -> Maybe Instrument
parseInstrument instrStr =
  if null instrStr || any (not . isValidInstrumentChar) instrStr
    then Nothing
    else Just (Instrument (T.pack instrStr))
  where
    isValidInstrumentChar c = c `elem` (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_-")

parseTimeRange :: String -> String -> String -> String -> Maybe DateRange
parseTimeRange startYearStr startMonthStr endYearStr endMonthStr = do
  startYear <- readMaybe startYearStr
  startMonth <- readMaybe startMonthStr
  endYear <- readMaybe endYearStr
  endMonth <- readMaybe endMonthStr
  if startMonth >= 1 && startMonth <= 12 && endMonth >= 1 && endMonth <= 12
    then Just $ DateRange startYear startMonth endYear endMonth
    else Nothing
