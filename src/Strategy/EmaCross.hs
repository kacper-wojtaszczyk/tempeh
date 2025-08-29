{-# LANGUAGE RecordWildCards #-}
module Strategy.EmaCross
  ( emaCrossStrategy
  , ema
  ) where

import Port.Strategy
import Domain.Types
import Data.Functor.Identity
import Data.Scientific (Scientific, fromFloatDigits)

-- Internal state for the EMA-strategy
data EmaState = EmaState
  { prevFast :: Maybe Price
  , prevSlow :: Maybe Price
  , prevDiff :: Maybe Scientific
  }

-- Public: construct an Identity-based (pure) EMA crossover strategy
emaCrossStrategy :: Int -> Int -> Strategy EmaState Identity
emaCrossStrategy fast slow = Strategy
  { initState = EmaState Nothing Nothing Nothing
  , step = \st candle ->
      let close = cClose candle
          newFast = computeEma fast (prevFast st) close
          newSlow = computeEma slow (prevSlow st) close
          diff = case (newFast, newSlow) of
            (Just (Price f), Just (Price s)) -> Just (f - s)
            _ -> Nothing
          sig = case (prevDiff st, diff) of
            (Just d1, Just d2) | d1 <= 0 && d2 > 0 -> Enter Buy
            (Just d1, Just d2) | d1 >= 0 && d2 < 0 -> Enter Sell
            _ -> Hold
          newSt = EmaState newFast newSlow diff
      in Identity (newSt, sig)
  }

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
