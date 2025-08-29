module Unit.Strategy.EmaCross.IndicatorsTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Strategy.EmaCross (ema)
import Domain.Types (Price(..))
import Data.Scientific (fromFloatDigits)

tests :: TestTree
tests = testGroup "Indicators"
  [ testCase "EMA of single element = element" $
      ema 5 [Price (fromFloatDigits 1.2345)]
        @?= [Price (fromFloatDigits 1.2345)]
  ]
