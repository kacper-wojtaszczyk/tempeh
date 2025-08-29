module Unit.Domain.TypesTest where

import Test.Tasty
import Test.Tasty.HUnit
import Domain.Types
import Data.Scientific (fromFloatDigits)

tests :: TestTree
tests = testGroup "Domain.Types"
  [ testCase "Price equality works" $
      Price (fromFloatDigits 1.23) @?= Price (fromFloatDigits 1.23)
  ]
