import Test.Tasty

import qualified Unit.Adapter.SimBrokerTest
import qualified Unit.Domain.TypesTest
import qualified Unit.Strategy.EmaCross.IndicatorsTest
import qualified Unit.Strategy.EmaCross.StrategyTest

main :: IO ()
main = defaultMain $ testGroup "All tests"
  [ Unit.Adapter.SimBrokerTest.tests
  , Unit.Domain.StrategyTest.tests
  , Unit.Domain.BrokerTest.tests
  , Unit.Strategy.EmaCross.StrategyTest.tests
  ]
