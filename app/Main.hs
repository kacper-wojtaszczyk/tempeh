module Main where

import System.Environment (getArgs)
import Application.Main (runBacktestWithNewArchitecture)
import Domain.Services.BacktestService (StrategyParameters(..))
import Domain.Types (Instrument(..))
import Port.DataProvider (DateRange(..))
import Text.Read (readMaybe)
import qualified Data.Text as T

main :: IO ()
main = do
  args <- getArgs

  case args of
    ["backtest", instrumentStr, startYearStr, startMonthStr, endYearStr, endMonthStr] -> do
      case (parseInstrument instrumentStr, parseTimeRange startYearStr startMonthStr endYearStr endMonthStr) of
        (Just instrument, Just dateRange) -> do
          let strategyParams = EmaCrossParams 5 20 0.0001  -- Default EMA parameters
          runBacktestWithNewArchitecture instrument dateRange strategyParams
        (Nothing, _) -> putStrLn $ "Invalid instrument format: " ++ instrumentStr ++ ". Use alphanumeric characters only."
        (_, Nothing) -> putStrLn "Invalid date format. Use: backtest <instrument> <start_year> <start_month> <end_year> <end_month>"
    [] -> printUsage
    _ -> do
      putStrLn "Invalid arguments."
      printUsage

printUsage :: IO ()
printUsage = do
  putStrLn "TEMPEH Trading Bot - Ports & Adapters Architecture"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  backtest <instrument> <start_year> <start_month> <end_year> <end_month>"
  putStrLn ""
  putStrLn "Parameters:"
  putStrLn "  instrument     - Currency pair (e.g., EURUSD, GBPUSD, USDJPY)"
  putStrLn "  start_year     - Start year (e.g., 2025)"
  putStrLn "  start_month    - Start month (1-12)"
  putStrLn "  end_year       - End year (e.g., 2025)"
  putStrLn "  end_month      - End month (1-12)"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  backtest EURUSD 2025 1 2025 3      - Run EURUSD backtest from Jan 2025 to Mar 2025"
  putStrLn "  backtest GBPUSD 2025 7 2025 8      - Run GBPUSD backtest from Jul 2025 to Aug 2025"
  putStrLn "  backtest USDJPY 2025 6 2025 6      - Run USDJPY backtest for June 2025 only"

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
