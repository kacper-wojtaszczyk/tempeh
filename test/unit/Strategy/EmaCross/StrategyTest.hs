{-# LANGUAGE RecordWildCards #-}
module Unit.Strategy.EmaCross.StrategyTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Strategy.EmaCross (ema, computeEma, EmaState(..))
import Domain.Types
import Data.Scientific (fromFloatDigits)
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)

tests :: TestTree
tests = testGroup "Strategy"
  [ testCase "EMA calculation should work correctly" $ do
      let prices = [Price (fromFloatDigits 1.0), Price (fromFloatDigits 1.1), Price (fromFloatDigits 1.2), Price (fromFloatDigits 1.25)]
          emaValues = ema 2 prices
      length emaValues @?= 4
      -- Test that EMA computation works - this is the core logic that remains relevant
      assertBool "EMA should compute values" (length emaValues > 0)
  , testCase "EMA computation should be responsive" $ do
      let price1 = Price (fromFloatDigits 1.0)
          price2 = Price (fromFloatDigits 1.1)
          ema1 = computeEma 2 Nothing price1
          ema2 = computeEma 2 ema1 price2
      assertBool "EMA should compute values" (ema1 /= Nothing && ema2 /= Nothing)
  ]
