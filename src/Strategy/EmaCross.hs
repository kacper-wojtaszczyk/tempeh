{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Strategy.EmaCross
  ( ema
  , EmaState(..)
  , computeEma
  , strategyProvider
  ) where

import GHC.Generics (Generic)
import qualified Data.Aeson as JSON
import Domain.Types
import Domain.Strategy (StrategyState(..))
import Application.Strategy.Types (StrategyInstance(..), StrategyParameters(..), SignalGenerator, StrategyProvider(..))
import Data.Scientific (Scientific, fromFloatDigits)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Typeable (cast)

-- Parameters for EMA strategy
data EmaParams = EmaParams
  { epFastPeriod :: Int
  , epSlowPeriod :: Int
  , epSignalThreshold :: Scientific
  } deriving (Show, Eq)

-- Internal EMA state
data EmaState = EmaState
  { prevFast :: Maybe Price
  , prevSlow :: Maybe Price
  , prevDiff :: Maybe Scientific
  } deriving (Show, Read, Eq, Generic)

instance JSON.ToJSON EmaState where
  toJSON = JSON.genericToJSON JSON.defaultOptions

instance JSON.FromJSON EmaState where
  parseJSON = JSON.genericParseJSON JSON.defaultOptions

-- Compute EMA helper
computeEma :: Int -> Maybe Price -> Price -> Maybe Price
computeEma _ Nothing p = Just p
computeEma n (Just (Price prev)) (Price cur) =
  let k = 2 / fromIntegral (n + 1) :: Double
      e = realToFrac (k * realToFrac cur + (1 - k) * realToFrac prev) :: Double
  in Just (Price (fromFloatDigits e))

-- Plain EMA over list (tests)
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

-- Provider
strategyProvider :: StrategyProvider
strategyProvider = StrategyProvider
  { spKeyword = "ema"
  , spName = "EMA Crossover"
  , spDescription = "Exponential Moving Average crossover strategy"
  , spFactory = createEmaInstance
  , spDefaultParams = defaultEmaParams
  , spParseParams = parseEmaParams
  , spValidateParams = validateEmaParams
  }

-- Default params (wrapped)
defaultEmaParams :: StrategyParameters
defaultEmaParams = StrategyParameters
  { spStrategyType = "ema"
  , spParameters = EmaParams 5 20 (fromFloatDigits 0.0001)
  , spValidator = \(EmaParams f s t) -> f > 0 && s > 0 && f /= s && t > 0 && t < 1
  , spParser = parseEmaParamsFromText
  , spDefaults = EmaParams 5 20 (fromFloatDigits 0.0001)
  }

-- Parse from CLI tokens
parseEmaParams :: [T.Text] -> Maybe StrategyParameters
parseEmaParams [] = Just defaultEmaParams
parseEmaParams params = do
  emaParams <- parseEmaParamsFromText params
  return $ StrategyParameters
    { spStrategyType = "ema"
    , spParameters = emaParams
    , spValidator = \(EmaParams f s t) -> f > 0 && s > 0 && f /= s && t > 0 && t < 1
    , spParser = parseEmaParamsFromText
    , spDefaults = EmaParams 5 20 (fromFloatDigits 0.0001)
    }

-- Adapter to internal parser
parseEmaParamsFromText :: [T.Text] -> Maybe EmaParams
parseEmaParamsFromText ts = parseEmaParamsInternal (map T.unpack ts)

parseEmaParamsInternal :: [String] -> Maybe EmaParams
parseEmaParamsInternal [fastStr, slowStr, thresholdStr] = do
  fast <- readMaybe fastStr
  slow <- readMaybe slowStr
  threshold <- readMaybe thresholdStr
  return $ EmaParams fast slow (fromFloatDigits threshold)
parseEmaParamsInternal _ = Nothing

-- Validate
validateEmaParams :: StrategyParameters -> Bool
validateEmaParams (StrategyParameters _ params validator _ _) = validator params

-- Factory using safe downcast
createEmaInstance :: StrategyParameters -> StrategyInstance
createEmaInstance (StrategyParameters _ params _ _ _) =
  case cast params :: Maybe EmaParams of
    Just (EmaParams fast slow threshold) -> StrategyInstance
      { siName = "EMA Crossover"
      , siDescription = T.pack $ "EMA(" <> show fast <> ") x EMA(" <> show slow <> ") with " <> show threshold <> " threshold"
      , siParameters = StrategyParameters "ema" (EmaParams fast slow threshold) (\(EmaParams f s t) -> f > 0 && s > 0 && f /= s && t > 0 && t < 1) parseEmaParamsFromText (EmaParams 5 20 (fromFloatDigits 0.0001))
      , siSignalGenerator = emaSignalGenerator fast slow threshold
      , siInitialState = serializeEmaState (EmaState Nothing Nothing Nothing)
      }
    Nothing -> error "Invalid EMA parameters type"

-- Signal generator
emaSignalGenerator :: Int -> Int -> Scientific -> SignalGenerator
emaSignalGenerator fastPeriod slowPeriod threshold candles state =
  case candles of
    [] -> (Hold, state)
    currentCandle:_ ->
      let prevState = deserializeEmaState state
          close = cClose currentCandle
          newFast = computeEma fastPeriod (prevFast prevState) close
          newSlow = computeEma slowPeriod (prevSlow prevState) close
          diff = case (newFast, newSlow) of
            (Just (Price f), Just (Price s)) -> Just (f - s)
            _ -> Nothing
          signal = case (prevDiff prevState, diff) of
            (Just d1, Just d2) ->
              if d1 <= 0 && d2 > threshold then Enter Buy
              else if d1 >= 0 && d2 < (-threshold) then Enter Sell
              else if d1 > threshold && d2 <= 0 then Exit
              else if d1 < (-threshold) && d2 >= 0 then Exit
              else Hold
            _ -> Hold
          newEmaState = EmaState newFast newSlow diff
        in (signal, serializeEmaState newEmaState)

-- State (de)serialization
deserializeEmaState :: StrategyState -> EmaState
deserializeEmaState strategyState =
  case reads (T.unpack $ unStrategyState strategyState) of
    [(emaState, "")] -> emaState
    _ -> EmaState Nothing Nothing Nothing

serializeEmaState :: EmaState -> StrategyState
serializeEmaState emaState = StrategyState { unStrategyState = T.pack $ show emaState }
