import Test.Tasty

-- Unit Tests (Some - Critical business logic)
import qualified Unit.Domain.TypesTest
import qualified Unit.Domain.ServicesTest
import qualified Unit.Strategy.EmaCross.IndicatorsTest
import qualified Unit.Strategy.EmaCross.StrategyTest
import qualified Unit.Adapter.CsvDataProviderTest
import qualified Unit.Adapter.RiskManagerTest

-- Integration Tests (Many - Component interactions, bulk of coverage)
import qualified Integration.BacktestIntegrationTest

-- E2E Tests (Few - Complete workflows)
import qualified E2E.CompleteBacktestTest

main :: IO ()
main = defaultMain $ testGroup "Tempeh Test Suite (Testing Diamond)"
  [ testGroup "🔺 End-to-End Tests (Few - Complete Workflows)"
    [ E2E.CompleteBacktestTest.tests
    ]
  , testGroup "🔶 Integration Tests (Many - Component Interactions - Bulk Coverage)"
    [ Integration.BacktestIntegrationTest.tests
    ]
  , testGroup "🔻 Unit Tests (Some - Critical Business Logic)"
    [ testGroup "Domain Layer"
      [ Unit.Domain.TypesTest.tests
      , Unit.Domain.ServicesTest.tests
      ]
    , testGroup "Strategy Layer"
      [ Unit.Strategy.EmaCross.IndicatorsTest.tests
      , Unit.Strategy.EmaCross.StrategyTest.tests
      ]
    , testGroup "Adapter Layer"
      [ Unit.Adapter.CsvDataProviderTest.tests
      , Unit.Adapter.RiskManagerTest.tests
      ]
    ]
  ]
