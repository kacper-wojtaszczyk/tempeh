{-# LANGUAGE OverloadedStrings #-}

module Unit.Strategy.TimingTest.StrategyTest where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import Data.Scientific (fromFloatDigits)
import qualified Data.Text as T

import Domain.Types
import Domain.Strategy (StrategyState(..))
import Strategy.TimingTest
import Application.Strategy.Types (StrategyInstance(..), StrategyParameters(..), SignalGenerator)

-- Test suite for TimingTest strategy
tests :: TestTree
tests = testGroup "TimingTest Strategy Tests"
  [ testEnterSignalAtFullMinute
  , testExitSignalAt30Seconds
  , testHoldBetweenSignals
  , testParameterParsing
  ]

-- Test that Enter signal is generated at full minute
testEnterSignalAtFullMinute :: TestTree
testEnterSignalAtFullMinute = testCase "Generate Enter signal at full minute" $ do
  let params = TimingTestParams Buy
      strategy = createTimingTestStrategy params
      signalGen = siSignalGenerator strategy
      initialState = siInitialState strategy

      -- Create a candle at exactly 12:34:00 (full minute)
      fullMinuteTime = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime (12 * 3600 + 34 * 60))  -- 12:34:00
      candle = Candle
        { cTime = fullMinuteTime
        , cOpen = Price (fromFloatDigits 1.1000)
        , cHigh = Price (fromFloatDigits 1.1010)
        , cLow = Price (fromFloatDigits 1.0990)
        , cClose = Price (fromFloatDigits 1.1005)
        }

  let (signal, _newState) = signalGen [candle] initialState
  assertEqual "Should generate Enter Buy signal at full minute" (Enter Buy) signal

-- Test that Exit signal is generated 30 seconds after full minute
testExitSignalAt30Seconds :: TestTree
testExitSignalAt30Seconds = testCase "Generate Exit signal 30 seconds after entry" $ do
  let params = TimingTestParams Sell
      strategy = createTimingTestStrategy params
      signalGen = siSignalGenerator strategy

      -- First, create entry at full minute
      fullMinuteTime = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime (12 * 3600 + 34 * 60))  -- 12:34:00
      entryCandle = Candle
        { cTime = fullMinuteTime
        , cOpen = Price (fromFloatDigits 1.1000)
        , cHigh = Price (fromFloatDigits 1.1010)
        , cLow = Price (fromFloatDigits 1.0990)
        , cClose = Price (fromFloatDigits 1.1005)
        }

      (enterSignal, stateAfterEntry) = signalGen [entryCandle] (siInitialState strategy)

      -- Then test exit at 30 seconds
      thirtySecondsTime = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime (12 * 3600 + 34 * 60 + 30))  -- 12:34:30
      exitCandle = Candle
        { cTime = thirtySecondsTime
        , cOpen = Price (fromFloatDigits 1.1005)
        , cHigh = Price (fromFloatDigits 1.1015)
        , cLow = Price (fromFloatDigits 1.0995)
        , cClose = Price (fromFloatDigits 1.1010)
        }

  assertEqual "Should first generate Enter Sell signal" (Enter Sell) enterSignal

  let (exitSignal, _finalState) = signalGen [exitCandle] stateAfterEntry
  assertEqual "Should generate Exit signal 30 seconds later" Exit exitSignal

-- Test that Hold signal is generated at other times
testHoldBetweenSignals :: TestTree
testHoldBetweenSignals = testCase "Generate Hold signal at non-trigger times" $ do
  let params = TimingTestParams Buy
      strategy = createTimingTestStrategy params
      signalGen = siSignalGenerator strategy
      initialState = siInitialState strategy

      -- Create a candle at 12:34:15 (15 seconds past minute)
      midMinuteTime = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime (12 * 3600 + 34 * 60 + 15))  -- 12:34:15
      candle = Candle
        { cTime = midMinuteTime
        , cOpen = Price (fromFloatDigits 1.1000)
        , cHigh = Price (fromFloatDigits 1.1010)
        , cLow = Price (fromFloatDigits 1.0990)
        , cClose = Price (fromFloatDigits 1.1005)
        }

  let (signal, _newState) = signalGen [candle] initialState
  assertEqual "Should generate Hold signal at mid-minute" Hold signal

-- Test parameter parsing
testParameterParsing :: TestTree
testParameterParsing = testCase "Parse strategy parameters correctly" $ do
  -- Test default parsing
  let defaultParsed = parseParams []
  assertEqual "Should parse default to Buy" (Just (TimingTestParams Buy)) defaultParsed

  -- Test explicit Buy
  let buyParsed = parseParams ["buy"]
  assertEqual "Should parse 'buy' correctly" (Just (TimingTestParams Buy)) buyParsed

  -- Test explicit Sell
  let sellParsed = parseParams ["sell"]
  assertEqual "Should parse 'sell' correctly" (Just (TimingTestParams Sell)) sellParsed

  -- Test case insensitive
  let buyUpperParsed = parseParams ["BUY"]
  assertEqual "Should parse 'BUY' correctly" (Just (TimingTestParams Buy)) buyUpperParsed

  -- Test invalid parameter
  let invalidParsed = parseParams ["invalid"]
  assertEqual "Should return Nothing for invalid parameter" Nothing invalidParsed

-- Helper function to create strategy (using the proper exported interface)
createTimingTestStrategy :: TimingTestParams -> StrategyInstance
createTimingTestStrategy params =
  let strategyParams = StrategyParameters "timing" params (\_ -> True) parseParams params
  in createTimingTestStrategyFromParams strategyParams
