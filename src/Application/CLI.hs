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
import Application.LiveTradingOrchestrator (orchestrateLiveTrading, defaultLiveTradingConfig, LiveTradingState(..))
import Application.Env (AppEnv(..), runAppM, runAppMWithLogging)
import Application.Strategy.Types (StrategyParameters(..), StrategyProvider(..))
import Application.Strategy.Factory (initializeStrategyRegistry)
import Application.Strategy.Registry (StrategyRegistry, listAvailableStrategies)
import Domain.Services.BacktestService (DateRange(..), BacktestResult(..))
import Domain.Types (Instrument(..))
import Util.Config (defaultAppConfig, acDataDirectory)
import Util.Logger (emptyLogContext, ComponentName(..))
import Data.Scientific (fromFloatDigits)
import Text.Read (readMaybe)
import qualified Data.Text as T

-- CLI Commands
data Command
  = BacktestCommand Instrument DateRange StrategyParameters
  | LiveCommand Instrument StrategyParameters  -- NEW: Live trading command
  | HelpCommand
  | InvalidCommand String
  deriving (Show, Eq)

-- Main CLI entry point
runCLI :: IO ()
runCLI = do
  args <- getArgs
  let registry = initializeStrategyRegistry
  case parseCommand registry args of
    BacktestCommand instrument dateRange strategyParams ->
      runBacktest instrument dateRange strategyParams
    LiveCommand instrument strategyParams ->  -- NEW: Handle live command
      runLiveTrading instrument strategyParams
    HelpCommand ->
      printUsage registry
    InvalidCommand err -> do
      putStrLn $ "Error: " ++ err
      printUsage registry

-- Refactored command parsing using abstract strategies
parseCommand :: StrategyRegistry -> [String] -> Command
parseCommand _ [] = HelpCommand
parseCommand _ ["--help"] = HelpCommand
parseCommand _ ["-h"] = HelpCommand

-- Generic strategy parsing using registry
parseCommand registry ("backtest":instrumentStr:startYearStr:startMonthStr:endYearStr:endMonthStr:strategyKeyword:params) =
  case findProvider strategyKeyword registry of
    Just provider ->
      case ( parseInstrument instrumentStr
           , parseTimeRange startYearStr startMonthStr endYearStr endMonthStr
           , parseStrategyParams provider params
           ) of
        (Just instrument, Just dateRange, Just strategyParams) ->
          BacktestCommand instrument dateRange strategyParams
        (Nothing, _, _) -> InvalidCommand $ "Invalid instrument format: " ++ instrumentStr
        (_, Nothing, _) -> InvalidCommand "Invalid date format"
        (_, _, Nothing) -> InvalidCommand $ "Invalid " ++ strategyKeyword ++ " parameters. See --help for parameter format"
    Nothing -> InvalidCommand $ "Unknown strategy: " ++ strategyKeyword ++ ". Available strategies: " ++ availableKeywords registry

-- NEW: Live trading command parsing
parseCommand registry ("live":instrumentStr:strategyKeyword:params) =
  case findProvider strategyKeyword registry of
    Just provider ->
      case ( parseInstrument instrumentStr
           , parseStrategyParams provider params
           ) of
        (Just instrument, Just strategyParams) ->
          LiveCommand instrument strategyParams
        (Nothing, _) -> InvalidCommand $ "Invalid instrument format: " ++ instrumentStr
        (_, Nothing) -> InvalidCommand $ "Invalid " ++ strategyKeyword ++ " parameters. See --help for parameter format"
    Nothing -> InvalidCommand $ "Unknown strategy: " ++ strategyKeyword ++ ". Available strategies: " ++ availableKeywords registry

-- Handle case where strategy is missing for live command
parseCommand registry ["live", instrumentStr] =
  InvalidCommand $ "Strategy is required for live trading. Available strategies: " ++ availableKeywords registry

parseCommand _ args = InvalidCommand $ "Invalid arguments: " ++ unwords args

-- Parse strategy parameters using provider
parseStrategyParams :: StrategyProvider -> [String] -> Maybe StrategyParameters
parseStrategyParams provider params =
  let toTexts = map T.pack params
  in case spParseParams provider toTexts of
       Just parsed -> if spValidateParams provider parsed then Just parsed else Nothing
       Nothing -> Just (spDefaultParams provider) -- Use defaults when none or invalid provided

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
        , aeLogContext  = emptyLogContext
        }
      btConfig = defaultBacktestConfig
        { bcInstrument = instrument
        , bcDateRange = dateRange
        , bcStrategyParams = strategyParams
        , bcInitialBalance = fromFloatDigits 10000.0
        , bcPositionSize = fromFloatDigits 1000.0
        }
      -- Create component name for logging based on backtest parameters
      componentName = ComponentName $ "backtest-" <> T.pack (show instrument) <> "-" <>
                                     spStrategyType strategyParams

  putStrLn $ "Logging to file with component: " <> show componentName
  result <- runAppMWithLogging componentName env (orchestrateBacktest btConfig)
  case result of
    Left err -> putStrLn $ "Backtest failed: " <> show err
    Right final -> do
      putStrLn "\n=== BACKTEST RESULTS ==="
      putStrLn $ "Final Balance: $" <> show (brFinalBalance final)
      putStrLn $ "Total P&L: $" <> show (brPnL final)
      putStrLn $ "Total Trades: " <> show (brTotalTrades final)

-- NEW: Live trading execution (now implemented)
runLiveTrading :: Instrument -> StrategyParameters -> IO ()
runLiveTrading instrument strategyParams = do
  putStrLn "=== TEMPEH LIVE TRADING ==="
  putStrLn $ "Instrument: " <> show instrument
  putStrLn $ "Strategy: " <> show strategyParams
  putStrLn ""
  putStrLn "Loading configuration and connecting to broker..."

  -- Create live trading configuration
  liveTradingConfig <- defaultLiveTradingConfig instrument strategyParams
  let registry = initializeStrategyRegistry

  -- Start live trading orchestration
  result <- orchestrateLiveTrading registry liveTradingConfig
  case result of
    Left err -> do
      putStrLn $ "Live trading failed: " <> show err
      putStrLn "Please check your configuration and broker connection."
    Right finalState -> do
      putStrLn "\n=== LIVE TRADING SESSION COMPLETED ==="
      putStrLn $ "Signals processed: " <> show (ltsSignalCount finalState)
      putStrLn $ "Last signal time: " <> show (ltsLastSignalTime finalState)
      putStrLn "Session ended."

-- Enhanced help function using strategy registry
printUsage :: StrategyRegistry -> IO ()
printUsage registry = do
  putStrLn "TEMPEH - Trading Strategy Backtester"
  putStrLn ""
  putStrLn "USAGE:"
  putStrLn "  tempeh backtest <INSTRUMENT> <START_YEAR> <START_MONTH> <END_YEAR> <END_MONTH> <STRATEGY> [STRATEGY_PARAMS...]"
  putStrLn "  tempeh live <INSTRUMENT> <STRATEGY> [STRATEGY_PARAMS...]"
  putStrLn "  tempeh --help"
  putStrLn ""
  putStrLn "ARGUMENTS:"
  putStrLn "  INSTRUMENT     Currency pair (e.g., EURUSD, GBPUSD)"
  putStrLn "  START_YEAR     Start year for backtest (e.g., 2025)"
  putStrLn "  START_MONTH    Start month for backtest (1-12)"
  putStrLn "  END_YEAR       End year for backtest (e.g., 2025)"
  putStrLn "  END_MONTH      End month for backtest (1-12)"
  putStrLn "  STRATEGY       Strategy name (see available strategies below)"
  putStrLn ""
  putStrLn "AVAILABLE STRATEGIES:"

  -- List available strategies from registry
  mapM_ printProvider (listAvailableStrategies registry)

  putStrLn ""
  putStrLn "EXAMPLES:"
  putStrLn "  tempeh backtest EURUSD 2025 1 2025 3 ema 5 20 0.0001"
  putStrLn "  tempeh backtest GBPUSD 2025 1 2025 6 rsi 14 70 30"
  putStrLn "  tempeh backtest EURUSD 2025 1 2025 3 ema  # uses defaults"
  putStrLn "  tempeh live EURUSD ema 5 20 0.0001"
  putStrLn "  tempeh live GBPUSD rsi 14 70 30"
  where
    printProvider provider = do
      putStrLn $ "  " ++ T.unpack (spKeyword provider) ++ " - " ++ T.unpack (spName provider)
      putStrLn $ "    " ++ T.unpack (spDescription provider)

-- Helpers
availableKeywords :: StrategyRegistry -> String
availableKeywords registry =
  unwords $ map (T.unpack . spKeyword) (listAvailableStrategies registry)

findProvider :: String -> StrategyRegistry -> Maybe StrategyProvider
findProvider keyword registry =
  let targets = listAvailableStrategies registry
  in case filter (\p -> spKeyword p == T.pack keyword) targets of
       (p:_) -> Just p
       _ -> Nothing

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
