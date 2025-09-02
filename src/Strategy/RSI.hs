{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module Strategy.RSI
  ( strategyProvider  -- New abstract provider export
  ) where

import Domain.Types
import Domain.Strategy (StrategyState(..))
import Application.Strategy.Types (StrategyInstance(..), StrategyParameters(..), SignalGenerator, StrategyProvider(..))
import Data.Scientific (Scientific, fromFloatDigits)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Typeable (cast)

-- Internal RSI strategy parameter type
data RSIParams = RSIParams
  { rsiPeriod :: Int
  , rsiOverbought :: Scientific
  , rsiOversold :: Scientific
  } deriving (Show, Eq)

-- RSI State to track gains and losses
data RSIState = RSIState
  { rsiPrevClose :: Maybe Price
  , rsiGains :: [Scientific]
  , rsiLosses :: [Scientific]
  , rsiPeriodInt :: Int
  } deriving (Show, Eq, Read)

-- Strategy Provider for RSI - this is what Application layer will use
strategyProvider :: StrategyProvider
strategyProvider = StrategyProvider
  { spKeyword = "rsi"
  , spName = "RSI Mean Reversion"
  , spDescription = "RSI-based mean reversion strategy"
  , spFactory = createRsiInstance
  , spDefaultParams = defaultRsiParams
  , spParseParams = parseRsiParams
  , spValidateParams = validateRsiParams
  }

-- Default RSI parameters wrapped in abstract type
defaultRsiParams :: StrategyParameters
defaultRsiParams = StrategyParameters
  { spStrategyType = "rsi"
  , spParameters = RSIParams 14 (fromFloatDigits 70.0) (fromFloatDigits 30.0)
  , spValidator = \(RSIParams p ob os) -> p > 1 && p <= 100 && ob > os && ob <= 100 && ob >= 50 && os >= 0 && os <= 50
  , spParser = parseRsiParamsFromText
  , spDefaults = RSIParams 14 (fromFloatDigits 70.0) (fromFloatDigits 30.0)
  }

-- Parse RSI parameters from text
parseRsiParams :: [T.Text] -> Maybe StrategyParameters
parseRsiParams [] = Just defaultRsiParams
parseRsiParams params = do
  rsiParams <- parseRsiParamsFromText params
  return $ StrategyParameters
    { spStrategyType = "rsi"
    , spParameters = rsiParams
    , spValidator = \(RSIParams p ob os) -> p > 1 && p <= 100 && ob > os && ob <= 100 && ob >= 50 && os >= 0 && os <= 50
    , spParser = parseRsiParamsFromText
    , spDefaults = RSIParams 14 (fromFloatDigits 70.0) (fromFloatDigits 30.0)
    }

-- Adapter: [Text] -> [String] -> RSIParams
parseRsiParamsFromText :: [T.Text] -> Maybe RSIParams
parseRsiParamsFromText ts = parseRsiParamsInternal (map T.unpack ts)

parseRsiParamsInternal :: [String] -> Maybe RSIParams
parseRsiParamsInternal [periodStr, overboughtStr, oversoldStr] = do
  period <- readMaybe periodStr
  overbought <- readMaybe overboughtStr
  oversold <- readMaybe oversoldStr
  return $ RSIParams period (fromFloatDigits overbought) (fromFloatDigits oversold)
parseRsiParamsInternal _ = Nothing

-- Validate RSI parameters
validateRsiParams :: StrategyParameters -> Bool
validateRsiParams (StrategyParameters _ params validator _ _) = validator params

-- Create RSI strategy instance from abstract parameters
createRsiInstance :: StrategyParameters -> StrategyInstance
createRsiInstance (StrategyParameters _ params _ _ _) =
  case cast params :: Maybe RSIParams of
    Just (RSIParams period overbought oversold) -> StrategyInstance
      { siName = "RSI Mean Reversion"
      , siDescription = T.pack $ "RSI(" <> show period <> ") with " <> show overbought <> "/" <> show oversold <> " levels"
      , siParameters = StrategyParameters "rsi" (RSIParams period overbought oversold) (\(RSIParams p ob os) -> p > 1 && p <= 100 && ob > os && ob <= 100 && ob >= 50 && os >= 0 && os <= 50) parseRsiParamsFromText (RSIParams 14 (fromFloatDigits 70.0) (fromFloatDigits 30.0))
      , siSignalGenerator = rsiSignalGenerator period overbought oversold
      , siInitialState = serializeRsiState (initialRSIState period)
      }
    Nothing -> error "Invalid RSI parameters type"

-- Initial empty RSI state
initialRSIState :: Int -> RSIState
initialRSIState period = RSIState
  { rsiPrevClose = Nothing
  , rsiGains = []
  , rsiLosses = []
  , rsiPeriodInt = period
  }

-- Calculate RSI value from current state
calculateRSI :: RSIState -> Maybe Scientific
calculateRSI state =
  let gains = rsiGains state
      losses = rsiLosses state
      period = rsiPeriodInt state
  in if length gains >= period && length losses >= period
     then let avgGain = realToFrac (sum (take period gains)) / fromIntegral period :: Double
              avgLoss = realToFrac (sum (take period losses)) / fromIntegral period :: Double
          in if avgLoss == 0
             then Just (fromFloatDigits 100.0)  -- All gains, RSI = 100
             else let rs = avgGain / avgLoss
                      rsi = 100 - (100 / (1 + rs))
                  in Just (fromFloatDigits rsi)
     else Nothing

-- Update RSI state with new price
updateRSIState :: RSIState -> Price -> RSIState
updateRSIState state (Price currentPrice) =
  case rsiPrevClose state of
    Nothing -> state { rsiPrevClose = Just (Price currentPrice) }
    Just (Price prevPrice) ->
      let change = realToFrac currentPrice - realToFrac prevPrice :: Double
          newGains = if change > 0 then fromFloatDigits change : rsiGains state else fromFloatDigits 0.0 : rsiGains state
          newLosses = if change < 0 then fromFloatDigits (abs change) : rsiLosses state else fromFloatDigits 0.0 : rsiLosses state
          period = rsiPeriodInt state
          -- Keep only the required number of periods
          trimmedGains = take (period + 1) newGains
          trimmedLosses = take (period + 1) newLosses
      in state
         { rsiPrevClose = Just (Price currentPrice)
         , rsiGains = trimmedGains
         , rsiLosses = trimmedLosses
         }

-- RSI signal generator for StrategyInstance
rsiSignalGenerator :: Int -> Scientific -> Scientific -> SignalGenerator
rsiSignalGenerator period overbought oversold candles state =
  case candles of
    [] -> (Hold, state)
    currentCandle:_ ->
      let prevState = deserializeRsiState state
          close = cClose currentCandle
          newState = updateRSIState prevState close
      in case calculateRSI newState of
        Nothing -> (Hold, serializeRsiState newState)  -- Not enough data yet
        Just rsi ->
          let signal
                | rsi <= oversold = Enter Buy    -- Oversold, buy signal
                | rsi >= overbought = Enter Sell -- Overbought, sell signal
                | rsi > oversold + fromFloatDigits 10.0 && rsi < overbought - fromFloatDigits 10.0 = Hold  -- Neutral zone
                | otherwise = Hold  -- Stay in current position
          in (signal, serializeRsiState newState)

-- Helper functions for RSI state management
deserializeRsiState :: StrategyState -> RSIState
deserializeRsiState strategyState =
  case reads (T.unpack $ unStrategyState strategyState) of
    [(rsiState, "")] -> rsiState
    _ -> initialRSIState 14

serializeRsiState :: RSIState -> StrategyState
serializeRsiState rsiState = StrategyState
  { unStrategyState = T.pack $ show rsiState
  }
