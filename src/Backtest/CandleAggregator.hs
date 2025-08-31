module Backtest.CandleAggregator
  ( aggregateTicksToCandles
  , ticksToCandlesWithPeriod
  , CandlePeriod(..)
  ) where

import Domain.Types
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Data.Scientific (Scientific)
import qualified Data.List as L

-- Candle aggregation periods
data CandlePeriod = OneMinute | FiveMinute | FifteenMinute | OneHour
  deriving (Show, Eq)

-- Convert period to seconds
periodToSeconds :: CandlePeriod -> Int
periodToSeconds OneMinute = 60
periodToSeconds FiveMinute = 300
periodToSeconds FifteenMinute = 900
periodToSeconds OneHour = 3600

-- Default: convert ticks to 1-minute candles (renamed for consistency)
aggregateTicksToCandles :: [Tick] -> [Candle]
aggregateTicksToCandles = ticksToCandlesWithPeriod OneMinute

-- Convert ticks to candles with specified period - pure function
ticksToCandlesWithPeriod :: CandlePeriod -> [Tick] -> [Candle]
ticksToCandlesWithPeriod _ [] = []
ticksToCandlesWithPeriod period ticks =
  let sortedTicks = L.sortOn tTime ticks
      periodSeconds = fromIntegral (periodToSeconds period)
      groupedTicks = groupTicksByPeriod periodSeconds sortedTicks
  in map tickGroupToCandle groupedTicks

-- Group ticks by time periods
groupTicksByPeriod :: Scientific -> [Tick] -> [[Tick]]
groupTicksByPeriod _ [] = []
groupTicksByPeriod periodSeconds (firstTick:restTicks) =
  let startTime = tTime firstTick
      (currentGroup, remaining) = takeTicksInPeriod startTime periodSeconds (firstTick:restTicks)
  in currentGroup : groupTicksByPeriod periodSeconds remaining

-- Take all ticks within a specific time period
takeTicksInPeriod :: UTCTime -> Scientific -> [Tick] -> ([Tick], [Tick])
takeTicksInPeriod startTime periodSeconds ticks =
  let endTime = addUTCTime (fromRational $ toRational periodSeconds) startTime
      (inPeriod, afterPeriod) = L.span (\tick -> tTime tick < endTime) ticks
  in (inPeriod, afterPeriod)

-- Convert a group of ticks to a single candle
tickGroupToCandle :: [Tick] -> Candle
tickGroupToCandle [] = error "Empty tick group" -- This should not happen with proper grouping
tickGroupToCandle ticks@(firstTick:_) =
  let sortedTicks = L.sortOn tTime ticks
      prices = map (midPrice . toBidAsk) sortedTicks
      openPrice = head prices
      closePrice = last prices
      highPrice = Price $ maximum $ map unPrice prices
      lowPrice = Price $ minimum $ map unPrice prices
  in Candle
    { cTime = tTime firstTick
    , cOpen = openPrice
    , cHigh = highPrice
    , cLow = lowPrice
    , cClose = closePrice
    }
  where
    toBidAsk tick = (tBid tick, tAsk tick)
    midPrice (bid, ask) = Price $ (unPrice bid + unPrice ask) / 2
