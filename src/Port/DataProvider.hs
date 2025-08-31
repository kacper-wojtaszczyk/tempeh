{-# LANGUAGE OverloadedStrings #-}
module Port.DataProvider where

import Domain.Types
import Util.Error (Result)
import Data.Time (UTCTime)
import Data.Scientific (Scientific)
import qualified Data.Text as T

-- Port for data access - abstracts away CSV, database, or live feeds
class Monad m => DataProvider m where
  loadTicks :: Instrument -> DateRange -> m (Result [Tick])
  loadCandles :: Instrument -> DateRange -> CandlePeriod -> m (Result [Candle])
  validateDataQuality :: [Tick] -> m (Result DataQualityReport)

-- Port for market data streaming (for live trading)
class Monad m => MarketDataStream m where
  subscribeTo :: Instrument -> m ()
  getLatestTick :: Instrument -> m (Maybe Tick)
  getLatestCandle :: Instrument -> CandlePeriod -> m (Maybe Candle)

-- Domain types for data operations
data DateRange = DateRange
  { drStartYear :: Int
  , drStartMonth :: Int
  , drEndYear :: Int
  , drEndMonth :: Int
  } deriving (Show, Eq)

data CandlePeriod = OneMinute | FiveMinute | FifteenMinute | OneHour | FourHour | Daily
  deriving (Show, Eq)

data DataQualityReport = DataQualityReport
  { dqrTickCount :: Int
  , dqrGapCount :: Int
  , dqrOutlierCount :: Int
  , dqrAverageSpread :: Scientific
  , dqrQualityScore :: Scientific  -- 0-100 score
  } deriving (Show, Eq)
