{-# LANGUAGE OverloadedStrings #-}
module Adapter.StrategyFactory where

import Domain.Services.BacktestService (StrategyInstance(..), StrategyParameters(..), StrategyState(..), SignalGenerator)
import Domain.Types
import Strategy.EmaCross (ema, EmaState(..), computeEma)
import Data.Scientific (Scientific, fromFloatDigits)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as JSON

-- Factory for creating strategy instances
createEmaStrategy :: Int -> Int -> Scientific -> StrategyInstance
createEmaStrategy fastPeriod slowPeriod threshold = StrategyInstance
  { siName = "EMA Crossover"
  , siDescription = T.pack $ "EMA(" <> show fastPeriod <> ") x EMA(" <> show slowPeriod <> ")"
  , siParameters = EmaCrossParams fastPeriod slowPeriod threshold
  , siSignalGenerator = emaSignalGenerator fastPeriod slowPeriod threshold
  }

createRSIStrategy :: Int -> Scientific -> Scientific -> StrategyInstance
createRSIStrategy period overbought oversold = StrategyInstance
  { siName = "RSI Mean Reversion"
  , siDescription = T.pack $ "RSI(" <> show period <> ") [" <> show oversold <> "-" <> show overbought <> "]"
  , siParameters = RSIParams period overbought oversold
  , siSignalGenerator = rsiSignalGenerator period overbought oversold
  }

-- EMA signal generator implementation
emaSignalGenerator :: Int -> Int -> Scientific -> SignalGenerator
emaSignalGenerator fastPeriod slowPeriod threshold candles state =
  case candles of
    [] -> (Hold, state)
    currentCandle:previousCandles ->
      let prevState = deserializeEmaState state
          close = cClose currentCandle
          newFast = computeEma fastPeriod (prevFast prevState) close
          newSlow = computeEma slowPeriod (prevSlow prevState) close

          diff = case (newFast, newSlow) of
            (Just (Price f), Just (Price s)) -> Just (f - s)
            _ -> Nothing

          signal = case (prevDiff prevState, diff) of
            -- Entry signals: Strong EMA crossover
            (Just d1, Just d2) | d1 <= 0 && d2 > threshold -> Enter Buy
            (Just d1, Just d2) | d1 >= 0 && d2 < (-threshold) -> Enter Sell
            -- Exit signals: Reverse crossover
            (Just d1, Just d2) | d1 > threshold && d2 <= 0 -> Exit
            (Just d1, Just d2) | d1 < (-threshold) && d2 >= 0 -> Exit
            _ -> Hold

          newEmaState = EmaState newFast newSlow diff
          newState = serializeEmaState newEmaState
      in (signal, newState)

-- RSI signal generator (simplified implementation)
rsiSignalGenerator :: Int -> Scientific -> Scientific -> SignalGenerator
rsiSignalGenerator period overbought oversold candles state =
  -- Simplified RSI implementation - would need proper RSI calculation
  (Hold, state)

-- State serialization for EMA (simplified)
serializeEmaState :: EmaState -> StrategyState
serializeEmaState emaState = StrategyState
  { ssInternal = T.pack $ show emaState  -- Simplified - would use JSON
  }

deserializeEmaState :: StrategyState -> EmaState
deserializeEmaState strategyState =
  case reads (T.unpack $ ssInternal strategyState) of
    [(emaState, "")] -> emaState
    _ -> EmaState Nothing Nothing Nothing  -- Default state if deserialization fails

-- Factory function that creates strategy from parameters
createStrategyFromConfig :: StrategyParameters -> StrategyInstance
createStrategyFromConfig (EmaCrossParams fast slow threshold) =
  createEmaStrategy fast slow threshold
createStrategyFromConfig (RSIParams period overbought oversold) =
  createRSIStrategy period overbought oversold
