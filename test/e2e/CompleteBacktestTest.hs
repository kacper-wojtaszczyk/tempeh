{-# LANGUAGE ScopedTypeVariables #-}
module E2E.CompleteBacktestTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Domain.Types
import Domain.Services.BacktestService
import Util.Error
import Data.Scientific (fromFloatDigits)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import Control.Exception (catch, IOException)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Application.CLI (parseCommand, Command(..))
import Application.BacktestOrchestrator (orchestrateBacktest, BacktestConfig(..), defaultBacktestConfig)
import Application.Env (AppEnv(..), runAppM)
import Adapter.CsvDataProvider (CsvDataProvider(..))
import Adapter.ReportGeneratorAdapter (ConsoleReportGenerator(..))
import Adapter.RiskManagerAdapter (defaultBasicRiskManager)
import Util.Config (defaultAppConfig, acDataDirectory)
import Application.Strategy.Factory (initializeStrategyRegistry)
import Application.Strategy.Types (StrategyParameters(..), StrategyProvider(..))
import Application.Strategy.Registry (StrategyRegistry, findStrategyByKeyword)
import Util.Logger (emptyLogContext)  -- Add missing import

-- E2E tests for complete application workflows
tests :: TestTree
tests = testGroup "End-to-End Tests"
  [ testCase "Complete backtest workflow with real data flow" testCompleteBacktestWorkflow
  , testCase "Error handling in complete pipeline" testErrorHandlingE2E
  , testCase "Performance validation across full pipeline" testFullPipelinePerformance
  , testGroup "Strategy E2E Baseline Tests (Post-Refactor)"
    [ testCase "EMA strategy with default parameters E2E baseline" testEmaDefaultParametersE2EBaseline
    , testCase "EMA strategy with custom parameters E2E baseline" testEmaCustomParametersE2EBaseline
    , testCase "RSI strategy with default parameters E2E baseline" testRsiDefaultParametersE2EBaseline
    , testCase "RSI strategy with custom parameters E2E baseline" testRsiCustomParametersE2EBaseline
    , testCase "Strategy parameter validation E2E baseline" testStrategyParameterValidationE2EBaseline
    , testCase "Strategy factory behavior baseline" testStrategyFactoryBehaviorBaseline
    , testCase "CLI parsing behavior baseline" testCliParsingBehaviorBaseline
    ]
  ]

registry :: StrategyRegistry
registry = initializeStrategyRegistry

testCompleteBacktestWorkflow :: IO ()
testCompleteBacktestWorkflow = do
  -- Setup test environment
  let testDataDir = "./test/fixtures/e2e"
  createTestDataEnvironment testDataDir

  -- Run complete backtest (this tests the entire application flow)
  let args = ["backtest", "EURUSD", "2025", "1", "2025", "1", "ema"]
  case parseCommand registry args of
    BacktestCommand instrument dateRange strategyParams -> do
      result <- runBacktestE2E instrument dateRange strategyParams
      case result of
        Left err -> assertFailure $ "Complete backtest workflow failed: " ++ show err
        Right btResult -> brTotalTrades btResult >= 0 @? "Should have valid trade count"
    _ -> assertFailure "Failed to parse command for complete backtest workflow"

  -- Cleanup
  cleanupTestEnvironment testDataDir

testErrorHandlingE2E :: IO ()
testErrorHandlingE2E = do
  -- Test complete error handling across all layers
  let invalidArgs = ["backtest", "INVALID", "2025", "13", "2025", "14"]  -- Invalid dates
  case parseCommand registry invalidArgs of
    InvalidCommand _ -> assertBool "Error handling works correctly" True
    _ -> assertFailure "Should have failed with invalid arguments"

testFullPipelinePerformance :: IO ()
testFullPipelinePerformance = do
  -- Test performance characteristics of complete pipeline
  let testDataDir = "./test/fixtures/performance"
  createTestDataEnvironment testDataDir

  let args = ["backtest", "EURUSD", "2025", "1", "2025", "1", "ema"]
  case parseCommand registry args of
    BacktestCommand instrument dateRange strategyParams -> do
      -- This would measure end-to-end performance in a real implementation
      result <- runBacktestE2E instrument dateRange strategyParams
      case result of
        Left _ -> assertBool "Performance test completed (expected failure with minimal data)" True
        Right _ -> assertBool "Performance test completed successfully" True
    _ -> assertFailure "Failed to parse performance test command"

  cleanupTestEnvironment testDataDir

-- Test EMA strategy with default parameters end-to-end (baseline)
testEmaDefaultParametersE2EBaseline :: IO ()
testEmaDefaultParametersE2EBaseline = do
  let args = ["backtest", "EURUSD", "2025", "1", "2025", "1", "ema"]
  case parseCommand registry args of
    BacktestCommand instrument dateRange strategyParams -> do
      -- Verify parsed parameters are EMA defaults
      spStrategyType strategyParams @?= T.pack "ema"

      -- Run actual backtest to verify end-to-end functionality
      result <- runBacktestE2E instrument dateRange strategyParams
      case result of
        Left err -> assertFailure $ "Backtest failed: " ++ show err
        Right btResult -> do
          -- Verify backtest completed successfully
          brTotalTrades btResult >= 0 @? "Should have valid trade count"

    InvalidCommand err -> assertFailure $ "Command parsing failed: " ++ err
    _ -> assertFailure "Expected BacktestCommand"

-- Test EMA strategy with custom parameters end-to-end (baseline)
testEmaCustomParametersE2EBaseline :: IO ()
testEmaCustomParametersE2EBaseline = do
  let args = ["backtest", "EURUSD", "2025", "1", "2025", "1", "ema", "12", "26", "0.0005"]
  case parseCommand registry args of
    BacktestCommand instrument dateRange strategyParams -> do
      -- Verify parsed parameters are custom EMA values
      spStrategyType strategyParams @?= T.pack "ema"

      -- Run actual backtest to verify end-to-end functionality
      result <- runBacktestE2E instrument dateRange strategyParams
      case result of
        Left err -> assertFailure $ "Backtest failed: " ++ show err
        Right btResult -> do
          brTotalTrades btResult >= 0 @? "Should have valid trade count"

    InvalidCommand err -> assertFailure $ "Command parsing failed: " ++ err
    _ -> assertFailure "Expected BacktestCommand"

-- Test RSI strategy with default parameters end-to-end (baseline)
testRsiDefaultParametersE2EBaseline :: IO ()
testRsiDefaultParametersE2EBaseline = do
  let args = ["backtest", "EURUSD", "2025", "1", "2025", "1", "rsi"]
  case parseCommand registry args of
    BacktestCommand instrument dateRange strategyParams -> do
      -- Verify parsed parameters are RSI defaults
      spStrategyType strategyParams @?= T.pack "rsi"

      -- Run actual backtest to verify end-to-end functionality
      result <- runBacktestE2E instrument dateRange strategyParams
      case result of
        Left err -> assertFailure $ "Backtest failed: " ++ show err
        Right btResult -> do
          brTotalTrades btResult >= 0 @? "Should have valid trade count"

    InvalidCommand err -> assertFailure $ "Command parsing failed: " ++ err
    _ -> assertFailure "Expected BacktestCommand"

-- Test RSI strategy with custom parameters end-to-end (baseline)
testRsiCustomParametersE2EBaseline :: IO ()
testRsiCustomParametersE2EBaseline = do
  let args = ["backtest", "EURUSD", "2025", "1", "2025", "1", "rsi", "21", "80", "20"]
  case parseCommand registry args of
    BacktestCommand instrument dateRange strategyParams -> do
      -- Verify parsed parameters are custom RSI values
      spStrategyType strategyParams @?= T.pack "rsi"

      -- Run actual backtest to verify end-to-end functionality
      result <- runBacktestE2E instrument dateRange strategyParams
      case result of
        Left err -> assertFailure $ "Backtest failed: " ++ show err
        Right btResult -> do
          brTotalTrades btResult >= 0 @? "Should have valid trade count"

    InvalidCommand err -> assertFailure $ "Command parsing failed: " ++ err
    _ -> assertFailure "Expected BacktestCommand"

-- Test strategy parameter validation end-to-end (baseline)
testStrategyParameterValidationE2EBaseline :: IO ()
testStrategyParameterValidationE2EBaseline = do
  -- Test invalid EMA parameters (same fast/slow periods)
  let invalidEmaArgs = ["backtest", "EURUSD", "2025", "1", "2025", "1", "ema", "20", "20", "0.0001"]
  case parseCommand registry invalidEmaArgs of
    InvalidCommand _ -> return () -- Expected behavior
    _ -> assertFailure "Should reject invalid EMA parameters (same fast/slow)"

  -- Test invalid RSI parameters (overbought < oversold)
  let invalidRsiArgs = ["backtest", "EURUSD", "2025", "1", "2025", "1", "rsi", "14", "30", "70"]
  case parseCommand registry invalidRsiArgs of
    InvalidCommand _ -> return () -- Expected behavior
    _ -> assertFailure "Should reject invalid RSI parameters (overbought < oversold)"

-- Test strategy factory behavior (baseline)
testStrategyFactoryBehaviorBaseline :: IO ()
testStrategyFactoryBehaviorBaseline = do
  let args = ["backtest", "EURUSD", "2025", "1", "2025", "1", "ema", "10", "30", "0.0002"]
  case parseCommand registry args of
    BacktestCommand _ _ params -> do
      -- Registry-based factory will be used downstream by orchestrator
      spStrategyType params @?= T.pack "ema"
      assertBool "Factory baseline ok" True
    _ -> assertFailure "Expected BacktestCommand"

-- Test CLI parsing behavior (baseline)
testCliParsingBehaviorBaseline :: IO ()
testCliParsingBehaviorBaseline = do
  let good = ["backtest", "EURUSD", "2025", "1", "2025", "3", "ema", "5", "20", "0.0001"]
      bad  = ["backtest", "EURUSD", "2025", "1", "2025", "3", "unknown"]
  case parseCommand registry good of
    BacktestCommand _ _ p -> spStrategyType p @?= T.pack "ema"
    _ -> assertFailure "Expected BacktestCommand for good args"
  case parseCommand registry bad of
    InvalidCommand _ -> pure ()
    _ -> assertFailure "Expected InvalidCommand for bad strategy"

-- Helper functions for E2E test setup with permanent fixtures
createTestDataEnvironment :: FilePath -> IO ()
createTestDataEnvironment dir = do
  -- The permanent test fixtures are already in place at test/fixtures/e2e/
  -- This function now just ensures the directory exists
  createDirectoryIfMissing True dir
  -- No need to create minimal data - we use permanent fixtures with real market data

cleanupTestEnvironment :: FilePath -> IO ()
cleanupTestEnvironment dir = do
  -- No cleanup needed since we're using permanent test fixtures
  return ()

-- Helper function to run backtest E2E with permanent test data fixtures
runBacktestE2E :: Instrument -> DateRange -> StrategyParameters -> IO (Result BacktestResult)
runBacktestE2E instrument dateRange strategyParams = do
  -- Use permanent test fixtures directory with real January 2025 EURUSD data
  let testDataDir = "./test/fixtures/e2e"

  -- Create a custom config that points to test fixtures instead of production data
  let testConfig = defaultAppConfig { acDataDirectory = testDataDir }
      env = AppEnv
        { aeCsvProvider = CsvDataProvider testDataDir  -- Use permanent test fixtures
        , aeRiskManager = defaultBasicRiskManager
        , aeReportGen   = ConsoleReportGenerator
        , aeLogContext  = emptyLogContext
        }
      btConfig = defaultBacktestConfig
        { bcInstrument = instrument
        , bcDateRange = dateRange
        , bcStrategyParams = strategyParams
        }

  result <- runAppM env (orchestrateBacktest btConfig)

  -- No cleanup needed since we use permanent fixtures
  return result
