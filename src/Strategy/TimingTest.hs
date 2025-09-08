{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Strategy.TimingTest
  ( TimingTestParams(..)
  , TimingTestState(..)
  , strategyProvider
  , createTimingTestStrategyFromParams
  , parseParams
  ) where

import GHC.Generics (Generic)
import qualified Data.Aeson as JSON
import Domain.Types
import Domain.Strategy (StrategyState(..))
import Application.Strategy.Types (StrategyInstance(..), StrategyParameters(..), SignalGenerator, StrategyProvider(..))
import Data.Scientific (Scientific, fromFloatDigits)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS
import Text.Read (readMaybe)
import Data.Typeable (cast)
import Data.Time (UTCTime, secondsToDiffTime, utctDayTime, DiffTime)

-- Parameters for TimingTest strategy
data TimingTestParams = TimingTestParams
  { ttpSide :: Side  -- Which side to trade (Buy or Sell)
  } deriving (Show, Eq)

-- Internal TimingTest state
data TimingTestState = TimingTestState
  { tsLastSignalTime :: Maybe UTCTime
  , tsPositionOpen :: Bool
  , tsLastCandle :: Maybe Candle
  } deriving (Show, Eq, Generic)

instance JSON.ToJSON TimingTestState where
  toJSON = JSON.genericToJSON JSON.defaultOptions

instance JSON.FromJSON TimingTestState where
  parseJSON = JSON.genericParseJSON JSON.defaultOptions

-- Convert UTCTime to seconds within the day
timeToSeconds :: UTCTime -> Int
timeToSeconds time =
  let dayTime = utctDayTime time
      seconds = floor (realToFrac dayTime :: Double)
  in seconds

-- Check if we're at a full minute (seconds = 0)
isFullMinute :: UTCTime -> Bool
isFullMinute time = (timeToSeconds time `mod` 60) == 0

-- Check if we're 30 seconds past a full minute
is30SecondsAfterMinute :: UTCTime -> Bool
is30SecondsAfterMinute time = (timeToSeconds time `mod` 60) == 30

-- Signal generator for TimingTest strategy
timingTestSignalGenerator :: TimingTestParams -> SignalGenerator
timingTestSignalGenerator params candles (StrategyState stateText) =
  case JSON.decode (LBS.fromStrict $ T.encodeUtf8 stateText) of
    Nothing -> (Hold, StrategyState (T.decodeUtf8 $ LBS.toStrict $ JSON.encode initialState))
    Just state ->
      case candles of
        [] -> (Hold, StrategyState (T.decodeUtf8 $ LBS.toStrict $ JSON.encode state))
        (latestCandle:_) ->
          let currentTime = cTime latestCandle
              currentState = state { tsLastCandle = Just latestCandle }

              -- Check if we should enter a position (full minute and no position open)
              shouldEnter = isFullMinute currentTime &&
                           not (tsPositionOpen currentState) &&
                           maybe True (\lastTime -> timeToSeconds currentTime /= timeToSeconds lastTime) (tsLastSignalTime currentState)

              -- Check if we should exit a position (30 seconds after entry and position is open)
              shouldExit = is30SecondsAfterMinute currentTime && tsPositionOpen currentState

          in if shouldEnter
             then
               let newState = currentState
                     { tsLastSignalTime = Just currentTime
                     , tsPositionOpen = True
                     }
               in (Enter (ttpSide params), StrategyState (T.decodeUtf8 $ LBS.toStrict $ JSON.encode newState))
             else if shouldExit
             then
               let newState = currentState
                     { tsLastSignalTime = Just currentTime
                     , tsPositionOpen = False
                     }
               in (Exit, StrategyState (T.decodeUtf8 $ LBS.toStrict $ JSON.encode newState))
             else
               (Hold, StrategyState (T.decodeUtf8 $ LBS.toStrict $ JSON.encode currentState))

-- Initial state
initialState :: TimingTestState
initialState = TimingTestState
  { tsLastSignalTime = Nothing
  , tsPositionOpen = False
  , tsLastCandle = Nothing
  }

-- Create strategy instance from StrategyParameters (required signature)
createTimingTestStrategyFromParams :: StrategyParameters -> StrategyInstance
createTimingTestStrategyFromParams (StrategyParameters _ params _ _ _) =
  case cast params of
    Just timingParams -> createTimingTestStrategy timingParams
    Nothing -> createTimingTestStrategy defaultParams  -- fallback

-- Create strategy instance from TimingTestParams
createTimingTestStrategy :: TimingTestParams -> StrategyInstance
createTimingTestStrategy params = StrategyInstance
  { siName = "TimingTest"
  , siDescription = "Simple timing-based strategy for testing - enters every full minute, exits 30 seconds later"
  , siParameters = StrategyParameters "timing" params validateParams parseParams params
  , siSignalGenerator = timingTestSignalGenerator params
  , siInitialState = StrategyState (T.decodeUtf8 $ LBS.toStrict $ JSON.encode initialState)
  }

-- Parameter validation
validateParams :: TimingTestParams -> Bool
validateParams _ = True  -- Always valid for this simple strategy

-- Parameter parsing from command line arguments
parseParams :: [T.Text] -> Maybe TimingTestParams
parseParams [] = Just $ TimingTestParams Buy  -- Default to Buy
parseParams ["buy"] = Just $ TimingTestParams Buy
parseParams ["sell"] = Just $ TimingTestParams Sell
parseParams [sideText] = case T.toLower sideText of
  "buy" -> Just $ TimingTestParams Buy
  "sell" -> Just $ TimingTestParams Sell
  _ -> Nothing
parseParams _ = Nothing

-- Default parameters
defaultParams :: TimingTestParams
defaultParams = TimingTestParams Buy

-- Strategy provider for registration
strategyProvider :: StrategyProvider
strategyProvider = StrategyProvider
  { spKeyword = "timing"
  , spName = "TimingTest"
  , spDescription = "Simple timing-based strategy for testing live broker integration"
  , spFactory = createTimingTestStrategyFromParams
  , spDefaultParams = StrategyParameters "timing" defaultParams validateParams parseParams defaultParams
  , spParseParams = \args -> Just $ StrategyParameters "timing"
                              (maybe defaultParams id (parseParams args))
                              validateParams parseParams defaultParams
  , spValidateParams = \(StrategyParameters _ params _ _ _) ->
                        case cast params of
                          Just timingParams -> validateParams timingParams
                          Nothing -> False
  }
