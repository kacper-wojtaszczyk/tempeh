import Test.Tasty

import qualified Unit.Domain.TypesTest
import qualified Unit.Domain.StrategyTest
import qualified Unit.Domain.BrokerTest
import qualified Unit.Application.RunnerTest

main :: IO ()
main = defaultMain $ testGroup "All tests"
  [ Unit.Domain.TypesTest.tests
  , Unit.Domain.StrategyTest.tests
  , Unit.Domain.BrokerTest.tests
  , Unit.Application.RunnerTest.tests
  ]
