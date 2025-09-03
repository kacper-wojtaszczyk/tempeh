{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Unit.Strategy.RSI.StrategyTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Strategy.RSI (strategyProvider)
import Application.Strategy.Types (StrategyProvider(..), StrategyParameters(..), StrategyInstance(..))
import Domain.Types
import Domain.Strategy (StrategyState(..))
import Data.Scientific (fromFloatDigits, toRealFloat)
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import qualified Data.Text as T

-- Test helper functions
testTime :: UTCTime
testTime = UTCTime (fromGregorian 2023 1 1) (secondsToDiffTime 0)

createCandle :: Double -> Double -> Double -> Double -> Candle
createCandle open high low close = Candle
  { cTime = testTime
  , cOpen = Price (fromFloatDigits open)
  , cHigh = Price (fromFloatDigits high)
  , cLow = Price (fromFloatDigits low)
  , cClose = Price (fromFloatDigits close)
  }

createSimpleCandle :: Double -> Candle
createSimpleCandle price = createCandle price price price price

-- Create test RSI strategy instances with different parameters
createRSIStrategy :: Int -> Double -> Double -> StrategyInstance
createRSIStrategy period overbought oversold =
  case spParseParams strategyProvider [T.pack (show period), T.pack (show overbought), T.pack (show oversold)] of
    Nothing -> error "Failed to create RSI strategy with valid parameters"
    Just params -> spFactory strategyProvider params

tests :: TestTree
tests = testGroup "RSI Strategy"
  [ testGroup "Strategy Creation"
    [ testCase "Create RSI strategy with default parameters" $ do
        let strategy = spFactory strategyProvider (spDefaultParams strategyProvider)
        T.isPrefixOf "RSI Mean Reversion" (siName strategy) @?= True
        T.isInfixOf "RSI(14)" (siDescription strategy) @?= True

    , testCase "Create RSI strategy with custom parameters" $ do
        let strategy = createRSIStrategy 21 75.0 25.0
        T.isInfixOf "RSI(21)" (siDescription strategy) @?= True
        T.isInfixOf "75.0/25.0" (siDescription strategy) @?= True
    ]

  , testGroup "RSI Signal Logic"
    [ testCase "RSI oversold condition generates buy signal" $ do
        let strategy = createRSIStrategy 5 70.0 30.0  -- Use shorter period for faster testing
            -- Create downward trend to generate low RSI (oversold)
            downtrend = [100, 99, 98, 97, 96, 95, 94, 93, 92, 91, 90]
            candles = map createSimpleCandle downtrend

        -- Process candles to build RSI state
        let processCandles [] state = (Hold, state)
            processCandles (c:cs) state =
              let (signal, newState) = siSignalGenerator strategy [c] state
              in if null cs
                 then (signal, newState)
                 else processCandles cs newState

            (finalSignal, _) = processCandles candles (siInitialState strategy)

        -- After strong downtrend, RSI should be low and generate buy signal
        finalSignal @?= Enter Buy

    , testCase "RSI overbought condition generates sell signal" $ do
        let strategy = createRSIStrategy 5 70.0 30.0  -- Use shorter period for faster testing
            -- Create upward trend to generate high RSI (overbought)
            uptrend = [90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100]
            candles = map createSimpleCandle uptrend

        -- Process candles to build RSI state
        let processCandles [] state = (Hold, state)
            processCandles (c:cs) state =
              let (signal, newState) = siSignalGenerator strategy [c] state
              in if null cs
                 then (signal, newState)
                 else processCandles cs newState

            (finalSignal, _) = processCandles candles (siInitialState strategy)

        -- After strong uptrend, RSI should be high and generate sell signal
        finalSignal @?= Enter Sell

    , testCase "RSI neutral zone generates hold signal" $ do
        let strategy = createRSIStrategy 14 70.0 30.0
            -- Create sideways market with small fluctuations
            sideways = [100, 100.1, 99.9, 100.05, 99.95, 100.02, 99.98]
            candles = map createSimpleCandle sideways

        -- Process all candles and check that most generate Hold signals
        let processAndCollectSignals [] state acc = reverse acc
            processAndCollectSignals (c:cs) state acc =
              let (signal, newState) = siSignalGenerator strategy [c] state
              in processAndCollectSignals cs newState (signal:acc)

            signals = processAndCollectSignals candles (siInitialState strategy) []
            holdCount = length $ filter (== Hold) signals

        -- Most signals in sideways market should be Hold
        assertBool "Most signals should be Hold in neutral conditions"
          (holdCount >= length signals `div` 2)
    ]

  , testGroup "RSI State Management"
    [ testCase "RSI state is preserved between signal generations" $ do
        let strategy = createRSIStrategy 14 70.0 30.0
            candle1 = createSimpleCandle 100.0
            candle2 = createSimpleCandle 101.0

            initialState = siInitialState strategy
            (_, state1) = siSignalGenerator strategy [candle1] initialState
            (_, state2) = siSignalGenerator strategy [candle2] state1

        -- States should be different, indicating progression
        state1 /= initialState @?= True
        state2 /= state1 @?= True

    , testCase "RSI handles price gaps correctly" $ do
        let strategy = createRSIStrategy 14 70.0 30.0
            -- Create a price gap scenario
            prices = [100.0, 100.5, 105.0, 104.5, 104.0]  -- Gap up then normalize
            candles = map createSimpleCandle prices

        -- Should handle gaps without crashing
        let processCandles [] state = state
            processCandles (c:cs) state =
              let (_, newState) = siSignalGenerator strategy [c] state
              in processCandles cs newState

            finalState = processCandles candles (siInitialState strategy)

        -- Should complete without error and maintain valid state
        finalState /= siInitialState strategy @?= True

    , testCase "RSI with identical prices handles zero change" $ do
        let strategy = createRSIStrategy 14 70.0 30.0
            -- All same price - should handle gracefully
            flatPrices = replicate 10 100.0
            candles = map createSimpleCandle flatPrices

        let processCandles [] state = state
            processCandles (c:cs) state =
              let (_, newState) = siSignalGenerator strategy [c] state
              in processCandles cs newState

            finalState = processCandles candles (siInitialState strategy)

        -- Should complete without error
        finalState /= siInitialState strategy @?= True
    ]

  , testGroup "RSI Edge Cases"
    [ testCase "RSI with single candle" $ do
        let strategy = createRSIStrategy 14 70.0 30.0
            candle = createSimpleCandle 100.0
            (signal, newState) = siSignalGenerator strategy [candle] (siInitialState strategy)

        -- Should generate Hold with insufficient data
        signal @?= Hold
        newState /= siInitialState strategy @?= True

    , testCase "RSI with extreme volatility" $ do
        let strategy = createRSIStrategy 5 70.0 30.0  -- Shorter period for quicker response
            -- Extreme price swings
            volatileData = [100, 110, 95, 105, 85, 115, 80, 120]
            candles = map createSimpleCandle volatileData

        -- Should handle extreme volatility without error
        let processCandles [] state = state
            processCandles (c:cs) state =
              let (_, newState) = siSignalGenerator strategy [c] state
              in processCandles cs newState

            finalState = processCandles candles (siInitialState strategy)

        finalState /= siInitialState strategy @?= True

    , testCase "RSI parameter boundary conditions" $ do
        -- Test with extreme but valid parameters
        let extremeStrategy = createRSIStrategy 2 99.0 1.0  -- Very sensitive settings
            candles = [createSimpleCandle 100.0, createSimpleCandle 101.0, createSimpleCandle 102.0]

        -- Should work with extreme but valid parameters
        let processCandles [] state = state
            processCandles (c:cs) state =
              let (_, newState) = siSignalGenerator extremeStrategy [c] state
              in processCandles cs newState

            finalState = processCandles candles (siInitialState extremeStrategy)

        finalState /= siInitialState extremeStrategy @?= True
    ]
  ]
