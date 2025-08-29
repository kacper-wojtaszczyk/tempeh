module Domain.Strategy where

import Domain.Types
import Data.Scientific (Scientific)

-- Simple EMA
ema :: Int -> [Price] -> [Price]
ema n = go Nothing
 where
  k = 2 / fromIntegral (n + 1) :: Double
  go _ [] = []
  go Nothing (Price p:xs) = let e = p in Price e : go (Just e) xs
  go (Just prev) (Price p:xs) =
    let e = realToFrac (k * realToFrac p + (1-k) * realToFrac prev)
    in Price e : go (Just e) xs

-- EMA crossover
emaCross :: Int -> Int -> [Candle] -> [(Candle, Signal)]
emaCross fast slow cs =
  let closes = map cClose cs
      f = ema fast closes
      s = ema slow closes
  in go (zip3 cs f s) Nothing
 where
  go [] _ = []
  go ((c, Price fe, Price se):xs) st =
    let sig = case st of
          Nothing -> Hold
          Just prev
            | prev <= se && fe > se  -> Enter Buy
            | prev >= se && fe < se  -> Enter Sell
            | otherwise              -> Hold
    in (c, sig) : go xs (Just fe)