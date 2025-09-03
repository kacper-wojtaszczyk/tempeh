import Test.Tasty

-- Unit Tests (Some - Critical business logic)
import qualified Unit.Domain.TypesTest
import qualified Unit.Domain.ServicesTest
import qualified Unit.Strategy.EmaCross.IndicatorsTest
import qualified Unit.Strategy.EmaCross.StrategyTest
import qualified Unit.Strategy.BollingerBands.IndicatorsTest
import qualified Unit.Strategy.BollingerBands.StrategyTest
import qualified Unit.Strategy.RSI.IndicatorsTest
import qualified Unit.Strategy.RSI.StrategyTest
import qualified Unit.Application.CLITest
import qualified Unit.Adapter.CsvDataProviderTest
import qualified Unit.Adapter.RiskManagerTest
import qualified Unit.Adapter.BacktestEngineTest
import qualified Unit.Util.ConfigTest
import qualified Unit.Util.ErrorTest
import qualified Unit.Util.LoggerTest

-- Integration Tests (Many - Component interactions, bulk of coverage)
import qualified Integration.BacktestIntegrationTest
import qualified Integration.StrategyComparisonTest

-- E2E Tests (Few - Complete workflows)
import qualified E2E.CompleteBacktestTest

main :: IO ()
main = defaultMain $ testGroup "Tempeh Test Suite (Testing Diamond)"
  [ testGroup "🔺 End-to-End Tests (Few - Complete Workflows)"
    [ E2E.CompleteBacktestTest.tests
    ]
  , testGroup "🔶 Integration Tests (Many - Component Interactions - Bulk Coverage)"
    [ Integration.BacktestIntegrationTest.tests
    , Integration.StrategyComparisonTest.tests
    ]
  , testGroup "🔻 Unit Tests (Some - Critical Business Logic)"
    [ testGroup "Domain Layer"
      [ Unit.Domain.TypesTest.tests
      , Unit.Domain.ServicesTest.tests
      ]
    , testGroup "Strategy Layer"
      [ Unit.Strategy.EmaCross.IndicatorsTest.tests
      , Unit.Strategy.EmaCross.StrategyTest.tests
      , Unit.Strategy.BollingerBands.IndicatorsTest.tests
      , Unit.Strategy.BollingerBands.StrategyTest.tests
      , Unit.Strategy.RSI.IndicatorsTest.tests
      , Unit.Strategy.RSI.StrategyTest.tests
      ]
    , testGroup "Application Layer"
      [ Unit.Application.CLITest.tests
      ]
    , testGroup "Adapter Layer"
      [ Unit.Adapter.CsvDataProviderTest.tests
      , Unit.Adapter.RiskManagerTest.tests
      , Unit.Adapter.BacktestEngineTest.tests
      ]
    , testGroup "Util Layer"
      [ Unit.Util.ConfigTest.tests
      , Unit.Util.ErrorTest.tests
      , Unit.Util.LoggerTest.tests
      ]
    ]
  ]
