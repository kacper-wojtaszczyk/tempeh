import Test.Tasty

-- Unit Tests (Some - Critical business logic)
import qualified Unit.Domain.TypesTest
import qualified Unit.Domain.ServicesTest
import qualified Unit.Domain.LiveDataServiceTest
import qualified Unit.Strategy.EmaCross.IndicatorsTest
import qualified Unit.Strategy.EmaCross.StrategyTest
import qualified Unit.Strategy.BollingerBands.IndicatorsTest
import qualified Unit.Strategy.BollingerBands.StrategyTest
import qualified Unit.Strategy.RSI.IndicatorsTest
import qualified Unit.Strategy.RSI.StrategyTest
import qualified Unit.Strategy.TimingTest.StrategyTest
import qualified Unit.Application.CLITest
import qualified Unit.Application.LiveTradingOrchestratorTest
import qualified Unit.Adapter.CsvDataProviderTest
import qualified Unit.Adapter.BrokerDataProviderTest
import qualified Unit.Adapter.RiskManagerTest
import qualified Unit.Adapter.BacktestEngineTest
import qualified Unit.Adapter.IG.PollingTest
import qualified Unit.Adapter.IG.StreamingTest
import qualified Unit.Adapter.IG.DealsTest
-- New modular IG adapter tests
import qualified Unit.Adapter.IG.SessionTest
import qualified Unit.Adapter.IG.ConnectionTest
import qualified Unit.Adapter.IG.ErrorTest
import qualified Unit.Adapter.IG.TradingTest
import qualified Unit.Util.ConfigTest
import qualified Unit.Util.ErrorTest
import qualified Unit.Util.LoggerTest

-- Integration Tests (Many - Component interactions, bulk of coverage)
import qualified Integration.BacktestIntegrationTest
import qualified Integration.StrategyComparisonTest
import qualified Integration.LiveTradingIntegrationTest
import qualified Integration.LiveTradingExecutionTest
import qualified Integration.Adapter.IG.DealsIntegrationTest

-- E2E Tests (Few - Complete workflows)
import qualified E2E.CompleteBacktestTest
import qualified E2E.LiveTradingE2ETest
import Util.Config (setTestConfigFromFile)

main :: IO ()
main = do
  -- Load testing-safe configuration (Demo broker, no network)
  e <- setTestConfigFromFile "config/test.json"
  case e of
    Left err -> error ("Failed to load test config: " <> show err)
    Right () -> return ()

  defaultMain $ testGroup "Tempeh Test Suite (Testing Diamond)"
    [ testGroup "\128314 End-to-End Tests (Few - Complete Workflows)"
      [ E2E.CompleteBacktestTest.tests
      , E2E.LiveTradingE2ETest.liveTradingE2ETests
      ]
    , testGroup "\128278 Integration Tests (Many - Component Interactions - Bulk Coverage)"
      [ Integration.BacktestIntegrationTest.tests
      , Integration.StrategyComparisonTest.tests
      , Integration.LiveTradingIntegrationTest.tests
      , Integration.LiveTradingExecutionTest.tests
      , Integration.Adapter.IG.DealsIntegrationTest.dealsIntegrationTests
      ]
    , testGroup "\128315 Unit Tests (Some - Critical Business Logic)"
      [ testGroup "Domain Layer"
        [ Unit.Domain.TypesTest.tests
        , Unit.Domain.ServicesTest.tests
        , Unit.Domain.LiveDataServiceTest.tests
        ]
      , testGroup "Strategy Layer"
        [ Unit.Strategy.EmaCross.IndicatorsTest.tests
        , Unit.Strategy.EmaCross.StrategyTest.tests
        , Unit.Strategy.BollingerBands.IndicatorsTest.tests
        , Unit.Strategy.BollingerBands.StrategyTest.tests
        , Unit.Strategy.RSI.IndicatorsTest.tests
        , Unit.Strategy.RSI.StrategyTest.tests
        , Unit.Strategy.TimingTest.StrategyTest.tests
        ]
      , testGroup "Application Layer"
        [ Unit.Application.CLITest.tests
        , Unit.Application.LiveTradingOrchestratorTest.tests
        ]
      , testGroup "Adapter Layer"
        [ Unit.Adapter.CsvDataProviderTest.tests
        , Unit.Adapter.BrokerDataProviderTest.tests
        , Unit.Adapter.RiskManagerTest.tests
        , Unit.Adapter.BacktestEngineTest.tests
        , Unit.Adapter.IG.PollingTest.tests
        , Unit.Adapter.IG.StreamingTest.tests
        , Unit.Adapter.IG.DealsTest.dealsTests
        -- New modular IG adapter tests
        , Unit.Adapter.IG.SessionTest.sessionTests
        , Unit.Adapter.IG.ConnectionTest.connectionTests
        , Unit.Adapter.IG.ErrorTest.errorTests
        , Unit.Adapter.IG.TradingTest.tradingTests
        ]
      , testGroup "Util Layer"
        [ Unit.Util.ConfigTest.tests
        , Unit.Util.ErrorTest.tests
        , Unit.Util.LoggerTest.tests
        ]
      ]
    ]
