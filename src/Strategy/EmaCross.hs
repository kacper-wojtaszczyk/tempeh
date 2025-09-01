{-# LANGUAGE RecordWildCards #-}
module Strategy.EmaCross
  ( emaCrossStrategy
  , emaCrossStrategyWithConfig
  , ema
  , EmaState(..)
  , computeEma
  ) where

import Domain.Strategy
import Domain.Types
import Strategy.Config (StrategyParameters(..), StrategyConfig(..))
import Data.Functor.Identity
import Data.Scientific (Scientific, fromFloatDigits)

-- Internal state for the EMA-strategy
data EmaState = EmaState
  { prevFast :: Maybe Price
  , prevSlow :: Maybe Price
  , prevDiff :: Maybe Scientific
  } deriving (Show, Read, Eq)

-- Legacy function for backward compatibility
emaCrossStrategy :: Int -> Int -> Strategy EmaState Identity
emaCrossStrategy fast slow = emaCrossStrategyWithConfig defaultConfig
  where
    defaultConfig = EmaCrossParams fast slow 0.0001

-- Enhanced strategy constructor with improved exit logic
emaCrossStrategyWithConfig :: StrategyParameters -> Strategy EmaState Identity
emaCrossStrategyWithConfig (EmaCrossParams fastPeriod slowPeriod threshold) = Strategy
  { initState = EmaState Nothing Nothing Nothing
  , step = \st candle ->
      let close = cClose candle
          newFast = computeEma fastPeriod (prevFast st) close
          newSlow = computeEma slowPeriod (prevSlow st) close
          diff = case (newFast, newSlow) of
            (Just (Price f), Just (Price s)) -> Just (f - s)
            _ -> Nothing
          sig = case (prevDiff st, diff) of
            -- Entry signals: Strong EMA crossover
            (Just d1, Just d2) | d1 <= 0 && d2 > threshold -> Enter Buy
            (Just d1, Just d2) | d1 >= 0 && d2 < (-threshold) -> Enter Sell
            -- Exit signals: Reverse crossover OR weakening trend
            (Just d1, Just d2) | d1 > threshold && d2 <= 0 -> Exit  -- Fast crosses back below slow
            (Just d1, Just d2) | d1 < (-threshold) && d2 >= 0 -> Exit  -- Fast crosses back above slow
            _ -> Hold
          newSt = EmaState newFast newSlow diff
      in Identity (newSt, sig)
  }
emaCrossStrategyWithConfig _ = error "Invalid strategy parameters for EMA Cross"

-- helper: compute next EMA value given previous EMA (or Nothing) and new price
computeEma :: Int -> Maybe Price -> Price -> Maybe Price
computeEma _ Nothing p = Just p
computeEma n (Just (Price prev)) (Price cur) =
  let k = 2 / fromIntegral (n + 1) :: Double
      e = realToFrac (k * realToFrac cur + (1 - k) * realToFrac prev) :: Double
  in Just (Price (fromFloatDigits e))

-- Exported EMA over a full list (useful in tests).
ema :: Int -> [Price] -> [Price]
ema _ [] = []
ema period (p:ps) = let first = p in first : go first ps
  where
    k = 2 / fromIntegral (period + 1) :: Double
    go _ [] = []
    go (Price prev) (Price x : xs) =
      let e = realToFrac (k * realToFrac x + (1 - k) * realToFrac prev) :: Double
          next = Price (fromFloatDigits e)
      in next : go (unPrice next `seq` next) xs
