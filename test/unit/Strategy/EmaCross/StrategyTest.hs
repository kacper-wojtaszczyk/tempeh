{-# LANGUAGE RecordWildCards #-}
module Unit.Strategy.EmaCross.StrategyTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Functor.Identity (Identity, runIdentity)
import Domain.Strategy
import Strategy.EmaCross (emaCrossStrategy)
import Domain.Types
import Data.Scientific (fromFloatDigits)
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)

-- small helper to make candles with given closes
mkCandle :: Int -> Double -> Candle
mkCandle i px = Candle
  { cTime = UTCTime (fromGregorian 2023 1 1) (secondsToDiffTime (fromIntegral i))
  , cOpen = p
  , cHigh = p
  , cLow  = p
  , cClose = p
  }
  where p = Price (fromFloatDigits px)

runStrategy :: Strategy s Identity -> [Candle] -> [Signal]
runStrategy Strategy{..} cs = snd $ foldl go (initState, []) cs
  where
    go (s, sigs) c =
      let (s', sig) = runIdentity (step s c)
      in (s', sigs ++ [sig])

tests :: TestTree
tests = testGroup "Strategy"
  [ testCase "EMA cross should produce Enter Buy when short > long" $ do
      let candles = [ mkCandle 0 1.0, mkCandle 1 1.1, mkCandle 2 1.2, mkCandle 3 1.25 ]
          strat = emaCrossStrategy 2 3
          sigs = runStrategy strat candles
      assertBool "should contain at least one Enter Buy" (any isBuy sigs)
  ]
  where
    isBuy (Enter Buy) = True
    isBuy _ = False
