import Test.Tasty

import qualified Unit.Domain.TypesTest
import qualified Unit.Strategy.EmaCross.IndicatorsTest
import qualified Unit.Strategy.EmaCross.StrategyTest

main :: IO ()
main = defaultMain $ testGroup "All tests"
  [ Unit.Domain.TypesTest.tests
  , Unit.Strategy.EmaCross.IndicatorsTest.tests
  , Unit.Strategy.EmaCross.StrategyTest.tests
  ]
