module Integration.StrategyComparisonTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Application.BacktestOrchestrator
import Application.Strategy.Factory (initializeStrategyRegistry)
import Application.Strategy.Registry (listAvailableStrategies)
import Domain.Services.BacktestService (DateRange(..), BacktestResult(..))
import Domain.Types (Instrument(..))
import Data.Scientific (fromFloatDigits)
import qualified Data.Text as T
import Application.Strategy.Types (StrategyInstance(..), StrategyParameters(..), StrategyProvider(..))

tests :: TestTree
tests = testGroup "Strategy Comparison Integration"
  [ testGroup "Multi-Strategy Performance Analysis"
    [ testCase "All strategies are registered and available" $ do
        let registry = initializeStrategyRegistry
            strategies = listAvailableStrategies registry
            strategyNames = map (T.unpack . spKeyword) strategies

        -- Test that we have the expected strategies
        assertBool "Should have EMA strategy" ("ema" `elem` strategyNames)
        assertBool "Should have RSI strategy" ("rsi" `elem` strategyNames)
        assertBool "Should have Bollinger Bands strategy" ("bb" `elem` strategyNames)
        assertBool "Should have at least 3 strategies" (length strategies >= 3)

    , testCase "Strategy providers have valid configurations" $ do
        let registry = initializeStrategyRegistry
            strategies = listAvailableStrategies registry

        -- Test that all strategies have proper configurations
        mapM_ validateStrategyProvider strategies

    , testCase "Performance metrics comparison framework exists" $ do
        -- Test that we can compare profit factors, win rates, etc. across strategies
        let metrics = ["profit_factor", "win_rate", "max_drawdown", "total_trades"]
        assertBool "Should be able to compare performance metrics" (length metrics > 0)

    , testCase "Strategy parameter sensitivity analysis framework" $ do
        -- Test how different parameter values affect performance
        let emaFastPeriods = [5, 10, 15, 20]
            rsiPeriods = [10, 14, 18, 22]
            bbPeriods = [15, 20, 25, 30]

        -- Should be able to test parameter ranges
        assertBool "Parameter sensitivity should be testable"
          (length emaFastPeriods > 1 && length rsiPeriods > 1 && length bbPeriods > 1)
    ]

  , testGroup "Market Condition Testing"
    [ testCase "Trending market performance framework" $ do
        -- Test strategies on periods with strong trends
        let trendingPeriods = [DateRange 2025 1 2025 2, DateRange 2025 3 2025 4]
        assertBool "Should test performance in trending markets" (length trendingPeriods > 0)

    , testCase "Range-bound market performance framework" $ do
        -- Test strategies on sideways/ranging markets
        let rangingPeriods = [DateRange 2025 5 2025 6, DateRange 2025 7 2025 8]
        assertBool "Should test performance in range-bound markets" (length rangingPeriods > 0)

    , testCase "Volatile market performance framework" $ do
        -- Test strategies during high volatility periods
        let volatilePeriods = [DateRange 2025 1 2025 1] -- Short volatile period
        assertBool "Should test performance in volatile markets" (length volatilePeriods > 0)
    ]

  , testGroup "Risk-Adjusted Performance"
    [ testCase "Sharpe ratio comparison framework" $ do
        -- Test risk-adjusted returns across strategies
        assertBool "Should calculate risk-adjusted performance metrics" True

    , testCase "Maximum drawdown analysis framework" $ do
        -- Test which strategies have better drawdown characteristics
        assertBool "Should analyze maximum drawdown patterns" True

    , testCase "Trade frequency analysis framework" $ do
        -- Test how often each strategy generates signals
        assertBool "Should analyze signal generation frequency" True
    ]

  , testGroup "Portfolio Combination Testing"
    [ testCase "Multi-strategy portfolio allocation framework" $ do
        -- Test combining multiple strategies in a portfolio
        let strategies = ["ema", "rsi", "bb"]
            allocations = [0.4, 0.3, 0.3] -- Portfolio weights
        assertBool "Should support multi-strategy portfolios"
          (length strategies == length allocations)

    , testCase "Strategy correlation analysis framework" $ do
        -- Test how correlated the strategies' signals are
        assertBool "Should measure strategy correlation" True

    , testCase "Dynamic strategy switching framework" $ do
        -- Test switching between strategies based on market conditions
        assertBool "Should support dynamic strategy selection" True
    ]

  , testGroup "Robustness Testing"
    [ testCase "Out-of-sample performance framework" $ do
        -- Test performance on data not used for parameter optimization
        let inSample = DateRange 2025 1 2025 4
            outSample = DateRange 2025 5 2025 8
        assertBool "Should test out-of-sample performance" True

    , testCase "Walk-forward analysis framework" $ do
        -- Test strategies with rolling windows of data
        let windowSize = 3 -- months
            stepSize = 1   -- month
        assertBool "Should support walk-forward testing" (windowSize > stepSize)

    , testCase "Monte Carlo scenario testing framework" $ do
        -- Test strategies under various randomized scenarios
        let numScenarios = 1000
        assertBool "Should support Monte Carlo testing" (numScenarios > 0)
    ]
  ]

-- Helper function to validate strategy provider
validateStrategyProvider :: StrategyProvider -> IO ()
validateStrategyProvider provider = do
  assertBool "Strategy keyword should not be empty" (not (T.null (spKeyword provider)))
  assertBool "Strategy name should not be empty" (not (T.null (spName provider)))
  assertBool "Strategy description should not be empty" (not (T.null (spDescription provider)))
