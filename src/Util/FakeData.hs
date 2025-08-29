module Util.FakeData (fakeCandles) where

import Domain.Types
import Data.Scientific (fromFloatDigits)
import Data.Time

-- Generate n toy candles (sine-like price movement)
fakeCandles :: Int -> [Candle]
fakeCandles n =
  let baseTime = UTCTime (fromGregorian 2023 1 1) 0
  in [ mkCandle i baseTime | i <- [0 .. n - 1] ]

mkCandle :: Int -> UTCTime -> Candle
mkCandle i t0 =
  let t = addUTCTime (fromIntegral (i * 60)) t0
      px = 1.10 + 0.01 * sin (fromIntegral i / 5)
      p = Price (fromFloatDigits px)
  in Candle t p p p p
