{-# LANGUAGE OverloadedStrings #-}
module Domain.Strategy where

import Domain.Types
import Data.Text (Text)

-- Pure domain abstraction for trading strategies
-- This contains only the essential domain concepts, not implementation details

-- A strategy signal generator that operates on market data
type SignalGenerator state = [Candle] -> state -> (Signal, state)

-- Abstract strategy interface - domain doesn't care about implementation details
data Strategy state = Strategy
  { strategyName :: Text
  , strategyDescription :: Text
  , generateSignal :: SignalGenerator state
  , initialState :: state
  }

-- Strategy state wrapper for serialization (implementation detail moved to Application)
newtype StrategyState = StrategyState { unStrategyState :: Text }
  deriving (Show, Eq)
