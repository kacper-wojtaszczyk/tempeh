{-# LANGUAGE OverloadedStrings #-}
module Debug.StrategyAnalyzer where

import Domain.Types
import Strategy.EmaCross (ema, EmaState(..), computeEma)
import Data.Scientific (Scientific)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Analyze EMA behavior on real data to debug strategy issues
analyzeEmaStrategy :: [Candle] -> IO ()
analyzeEmaStrategy candles = do
  putStrLn "=== EMA STRATEGY ANALYSIS ==="
  putStrLn $ "Total candles: " <> show (length candles)

  let prices = map cClose candles
      fastPeriod = 5
      slowPeriod = 20

  if length candles >= slowPeriod
    then do
      let fastEmas = ema fastPeriod prices
          slowEmas = ema slowPeriod prices
          diffs = zipWith (\(Price f) (Price s) -> f - s) fastEmas slowEmas

      putStrLn $ "First 10 EMA differences:"
      mapM_ print (take 10 diffs)

      putStrLn $ "Last 10 EMA differences:"
      mapM_ print (take 10 $ reverse diffs)

      -- Count crossovers
      let crossovers = countCrossovers diffs
      putStrLn $ "Total crossovers detected: " <> show crossovers

      -- Show extreme differences
      let maxDiff = maximum diffs
          minDiff = minimum diffs
      putStrLn $ "Max difference: " <> show maxDiff
      putStrLn $ "Min difference: " <> show minDiff

    else putStrLn "Not enough candles for analysis"

-- Count how many times the EMA difference changes sign
countCrossovers :: [Scientific] -> Int
countCrossovers [] = 0
countCrossovers [_] = 0
countCrossovers (d1:d2:rest) =
  let crossover = if (d1 > 0 && d2 <= 0) || (d1 <= 0 && d2 > 0) then 1 else 0
  in crossover + countCrossovers (d2:rest)
