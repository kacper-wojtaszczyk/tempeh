{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module Strategy.BollingerBands
  ( strategyProvider
  , calculateSMA
  , calculateStdDev
  , generateSignalFromBands
  , BBParams(..)
  , BBState(..)
  ) where

import Domain.Types
import Domain.Strategy (StrategyState(..))
import Application.Strategy.Types (StrategyInstance(..), StrategyParameters(..), SignalGenerator, StrategyProvider(..))
import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Typeable (cast)

-- Bollinger Bands parameters
data BBParams = BBParams
  { bbPeriod :: Int
  , bbStdDevMultiplier :: Scientific
  , bbSignalThreshold :: Scientific  -- Minimum distance from band to generate signal
  } deriving (Show, Eq)

-- Bollinger Bands state to track price history
data BBState = BBState
  { bbPriceHistory :: [Price]
  , bbPeriodInt :: Int
  } deriving (Show, Eq, Read)

-- Strategy Provider for Bollinger Bands
strategyProvider :: StrategyProvider
strategyProvider = StrategyProvider
  { spKeyword = "bb"
  , spName = "Bollinger Bands"
  , spDescription = "Bollinger Bands mean reversion strategy"
  , spFactory = createBBInstance
  , spDefaultParams = defaultBBParams
  , spParseParams = parseBBParams
  , spValidateParams = validateBBParams
  }

-- Default Bollinger Bands parameters
defaultBBParams :: StrategyParameters
defaultBBParams = StrategyParameters
  { spStrategyType = "bb"
  , spParameters = BBParams 20 (fromFloatDigits 2.0) (fromFloatDigits 0.0001)
  , spValidator = \(BBParams p mult thresh) -> p > 1 && mult > 0 && thresh >= 0
  , spParser = parseBBParamsFromText
  , spDefaults = BBParams 20 (fromFloatDigits 2.0) (fromFloatDigits 0.0001)
  }

-- Parse BB parameters from command line arguments
parseBBParams :: [T.Text] -> Maybe StrategyParameters
parseBBParams args = do
  params <- parseBBParamsFromText args
  return $ StrategyParameters
    { spStrategyType = "bb"
    , spParameters = params
    , spValidator = \(BBParams p mult thresh) -> p > 1 && mult > 0 && thresh >= 0
    , spParser = parseBBParamsFromText
    , spDefaults = BBParams 20 (fromFloatDigits 2.0) (fromFloatDigits 0.0001)
    }

-- Parse BB parameters from text arguments
parseBBParamsFromText :: [T.Text] -> Maybe BBParams
parseBBParamsFromText [] = Just $ BBParams 20 (fromFloatDigits 2.0) (fromFloatDigits 0.0001)
parseBBParamsFromText [p] = do
  period <- readMaybe (T.unpack p)
  return $ BBParams period (fromFloatDigits 2.0) (fromFloatDigits 0.0001)
parseBBParamsFromText [p, m] = do
  period <- readMaybe (T.unpack p)
  mult <- readMaybe (T.unpack m)
  return $ BBParams period (fromFloatDigits mult) (fromFloatDigits 0.0001)
parseBBParamsFromText [p, m, t] = do
  period <- readMaybe (T.unpack p)
  mult <- readMaybe (T.unpack m)
  thresh <- readMaybe (T.unpack t)
  return $ BBParams period (fromFloatDigits mult) (fromFloatDigits thresh)
parseBBParamsFromText _ = Nothing

-- Validate BB parameters
validateBBParams :: StrategyParameters -> Bool
validateBBParams (StrategyParameters _ params _ _ _) =
  case cast params of
    Just (BBParams p mult thresh) -> p > 1 && mult > 0 && thresh >= 0
    Nothing -> False

-- Create Bollinger Bands strategy instance
createBBInstance :: StrategyParameters -> StrategyInstance
createBBInstance params@(StrategyParameters _ bbParams _ _ _) =
  case cast bbParams of
    Just p@(BBParams period _ _) ->
      StrategyInstance
        { siName = "Bollinger Bands"
        , siDescription = "Mean reversion strategy using Bollinger Bands"
        , siParameters = params
        , siSignalGenerator = generateBBSignal p
        , siInitialState = serializeBBState (initialBBState period)
        }
    Nothing -> error "Invalid BB parameters in createBBInstance"

-- Initial empty BB state
initialBBState :: Int -> BBState
initialBBState period = BBState
  { bbPriceHistory = []
  , bbPeriodInt = period
  }

-- Serialize BB state to StrategyState
serializeBBState :: BBState -> StrategyState
serializeBBState state = StrategyState (T.pack (show state))

-- Deserialize StrategyState to BB state
deserializeBBState :: StrategyState -> Maybe BBState
deserializeBBState (StrategyState stateText) = readMaybe (T.unpack stateText)

-- Generate Bollinger Bands signal
generateBBSignal :: BBParams -> SignalGenerator
generateBBSignal params@(BBParams period stdMult threshold) candles state =
  case deserializeBBState state of
    Nothing -> (Hold, state)  -- Invalid state, hold position
    Just bbState ->
      case candles of
        [] -> (Hold, state)
        (c:_) ->
          let currentPrice = cClose c
              newHistory = take period (currentPrice : bbPriceHistory bbState)
              newState = BBState newHistory period
              signal = if length newHistory >= period
                      then generateSignalFromBands params currentPrice newHistory
                      else Hold
          in (signal, serializeBBState newState)

-- Generate signal based on Bollinger Bands calculation
generateSignalFromBands :: BBParams -> Price -> [Price] -> Signal
generateSignalFromBands (BBParams _ stdMult threshold) currentPrice priceHistory =
  let sma = calculateSMA priceHistory
      stdDev = calculateStdDev priceHistory sma
      stdMultDouble = realToFrac stdMult  -- Convert Scientific to Double for multiplication
      thresholdDouble = realToFrac threshold  -- Convert Scientific to Double
      currentPriceDouble = realToFrac (unPrice currentPrice)
      smaDouble = realToFrac (unPrice sma)
  in
      -- Handle edge case where standard deviation is zero (all prices identical)
      if stdDev == 0.0
      then
        let distanceFromSMA = abs (currentPriceDouble - smaDouble)
        in if currentPriceDouble < smaDouble && distanceFromSMA >= thresholdDouble
           then Enter Buy  -- Price below SMA by at least threshold distance
           else if currentPriceDouble > smaDouble && distanceFromSMA >= thresholdDouble
           then Enter Sell  -- Price above SMA by at least threshold distance
           else Hold
      else
        -- Normal case with non-zero standard deviation
        -- Apply threshold as a multiplier to the band width for more reasonable signal generation
        let bandWidth = stdMultDouble * stdDev
            adjustedBandWidth = max bandWidth thresholdDouble  -- Use threshold as minimum band width
            upperBand = smaDouble + adjustedBandWidth
            lowerBand = smaDouble - adjustedBandWidth
        in if currentPriceDouble <= lowerBand
           then Enter Buy  -- Price at or below lower band
           else if currentPriceDouble >= upperBand
           then Enter Sell  -- Price at or above upper band
           else Hold

-- Calculate Simple Moving Average
calculateSMA :: [Price] -> Price
calculateSMA prices =
  let priceDoubles = map (realToFrac . unPrice) prices
      sum' = sum priceDoubles
      count = fromIntegral (length prices)
      average = sum' / count
  in Price (fromFloatDigits average)  -- Keep original precision, don't round aggressively

-- Calculate Standard Deviation
calculateStdDev :: [Price] -> Price -> Double
calculateStdDev prices (Price sma) =
  let smaDouble = realToFrac sma
      priceDoubles = map (realToFrac . unPrice) prices
      deviations = map (\p -> (p - smaDouble) ** 2) priceDoubles
      variance = sum deviations / fromIntegral (length prices)
  in sqrt variance  -- Keep original precision for stdDev too
