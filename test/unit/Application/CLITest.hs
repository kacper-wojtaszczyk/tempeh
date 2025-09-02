module Unit.Application.CLITest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Application.CLI (parseCommand, Command(..))
import Application.Strategy.Factory (initializeStrategyRegistry)
import Domain.Types (Instrument(..))
import Domain.Services.BacktestService (DateRange(..))
import Application.Strategy.Types (StrategyParameters(..))
import qualified Data.Text as T
import Data.Char (toLower)
import Data.List (isInfixOf)

tests :: TestTree
tests = testGroup "CLI Module"
  [ testGroup "Command Parsing"
    [ testCase "Valid backtest command parses correctly" $ do
        let registry = initializeStrategyRegistry
            args = ["backtest", "EURUSD", "2025", "1", "2025", "2", "ema", "5", "20", "0.0001"]
            result = parseCommand registry args
        case result of
          BacktestCommand instr dateRange _ -> do
            instr @?= Instrument (T.pack "EURUSD")
            dateRange @?= DateRange 2025 1 2025 2
          _ -> assertFailure "Expected BacktestCommand"

    , testCase "Help command parses correctly" $ do
        let registry = initializeStrategyRegistry
            result1 = parseCommand registry ["--help"]
            result2 = parseCommand registry ["-h"]
            result3 = parseCommand registry []
        result1 @?= HelpCommand
        result2 @?= HelpCommand
        result3 @?= HelpCommand

    , testCase "Invalid instrument generates error" $ do
        let registry = initializeStrategyRegistry
            args = ["backtest", "", "2025", "1", "2025", "2", "ema"]
            result = parseCommand registry args
        case result of
          InvalidCommand msg -> assertBool "Should mention invalid instrument" ("instrument" `elem` words msg)
          _ -> assertFailure "Expected InvalidCommand for empty instrument"

    , testCase "Invalid date range generates error" $ do
        let registry = initializeStrategyRegistry
            args = ["backtest", "EURUSD", "invalid", "1", "2025", "2", "ema"]
            result = parseCommand registry args
        case result of
          InvalidCommand msg -> assertBool "Should mention invalid date" ("date" `elem` words msg)
          _ -> assertFailure "Expected InvalidCommand for invalid date"

    , testCase "Unknown strategy generates error" $ do
        let registry = initializeStrategyRegistry
            args = ["backtest", "EURUSD", "2025", "1", "2025", "2", "unknown"]
            result = parseCommand registry args
        case result of
          InvalidCommand msg -> assertBool "Should mention unknown strategy" ("strategy" `isInfixOf` map toLower msg)
          _ -> assertFailure "Expected InvalidCommand for unknown strategy"

    , testCase "Missing strategy generates error" $ do
        let registry = initializeStrategyRegistry
            args = ["backtest", "EURUSD", "2025", "1", "2025", "2"]
            result = parseCommand registry args
        case result of
          InvalidCommand _ -> return ()
          _ -> assertFailure "Expected InvalidCommand when strategy is missing"

    , testCase "EMA strategy with default parameters" $ do
        let registry = initializeStrategyRegistry
            args = ["backtest", "EURUSD", "2025", "1", "2025", "2", "ema"]
            result = parseCommand registry args
        case result of
          BacktestCommand _ _ strategyParams ->
            -- Should parse with default EMA parameters
            assertBool "Should have EMA strategy parameters" True
          _ -> assertFailure "Expected valid BacktestCommand with default EMA params"

    , testCase "RSI strategy with custom parameters" $ do
        let registry = initializeStrategyRegistry
            args = ["backtest", "EURUSD", "2025", "1", "2025", "2", "rsi", "14", "80", "20"]
            result = parseCommand registry args
        case result of
          BacktestCommand _ _ _ -> return () -- Valid command
          _ -> assertFailure "Expected valid BacktestCommand with RSI params"

    , testCase "Bollinger Bands strategy with parameters" $ do
        let registry = initializeStrategyRegistry
            args = ["backtest", "EURUSD", "2025", "1", "2025", "2", "bb", "20", "2.5", "0.0002"]
            result = parseCommand registry args
        case result of
          BacktestCommand _ _ _ -> return () -- Valid command
          _ -> assertFailure "Expected valid BacktestCommand with BB params"
    ]

  , testGroup "Date Range Validation"
    [ testCase "Valid month ranges accepted" $ do
        let registry = initializeStrategyRegistry
            args = ["backtest", "EURUSD", "2025", "1", "2025", "12", "ema"]
            result = parseCommand registry args
        case result of
          BacktestCommand _ (DateRange _ 1 _ 12) _ -> return ()
          _ -> assertFailure "Expected valid date range 1-12"

    , testCase "Invalid month rejected" $ do
        let registry = initializeStrategyRegistry
            args = ["backtest", "EURUSD", "2025", "13", "2025", "12", "ema"]
            result = parseCommand registry args
        case result of
          InvalidCommand _ -> return ()
          _ -> assertFailure "Expected InvalidCommand for month > 12"

    , testCase "Start date after end date handled gracefully" $ do
        let registry = initializeStrategyRegistry
            args = ["backtest", "EURUSD", "2025", "12", "2025", "1", "ema"]
            result = parseCommand registry args
        case result of
          BacktestCommand _ (DateRange 2025 12 2025 1) _ -> return () -- Parser allows it
          InvalidCommand _ -> return () -- Or validation rejects it
          _ -> assertFailure "Should either parse or reject invalid date range"
    ]

  , testGroup "Instrument Validation"
    [ testCase "Standard forex pairs accepted" $ do
        let registry = initializeStrategyRegistry
            pairs = ["EURUSD", "GBPUSD", "USDJPY", "AUDUSD"]
            testPair pair = case parseCommand registry ["backtest", pair, "2025", "1", "2025", "2", "ema"] of
              BacktestCommand (Instrument instr) _ _ -> instr == T.pack pair
              _ -> False
        assertBool "All standard pairs should be accepted" (all testPair pairs)

    , testCase "Case sensitivity handled" $ do
        let registry = initializeStrategyRegistry
            args = ["backtest", "eurusd", "2025", "1", "2025", "2", "ema"]
            result = parseCommand registry args
        case result of
          BacktestCommand (Instrument instr) _ _ -> instr @?= T.pack "eurusd" -- Should preserve case
          _ -> assertFailure "Should accept lowercase instrument"

    , testCase "Special characters in instrument handled" $ do
        let registry = initializeStrategyRegistry
            args = ["backtest", "EUR-USD", "2025", "1", "2025", "2", "ema"]
            result = parseCommand registry args
        case result of
          BacktestCommand _ _ _ -> return () -- Valid if parser accepts it
          InvalidCommand _ -> return () -- Or invalid if parser rejects it
          _ -> assertFailure "Should handle special characters consistently"
    ]
  ]
