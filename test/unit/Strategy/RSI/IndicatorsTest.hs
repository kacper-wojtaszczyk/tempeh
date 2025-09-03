{-# LANGUAGE OverloadedStrings #-}
module Unit.Strategy.RSI.IndicatorsTest (tests) where

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

createCandle :: Double -> Candle
createCandle price = Candle
  { cTime = testTime
  , cOpen = Price (fromFloatDigits price)
  , cHigh = Price (fromFloatDigits price)
  , cLow = Price (fromFloatDigits price)
  , cClose = Price (fromFloatDigits price)
  }

-- Create test RSI strategy instance for testing
createTestRSIInstance :: StrategyInstance
createTestRSIInstance =
  let defaultParams = spDefaultParams strategyProvider
  in spFactory strategyProvider defaultParams

tests :: TestTree
tests = testGroup "RSI Indicators"
  [ testGroup "RSI Strategy Provider"
    [ testCase "Strategy provider has correct metadata" $ do
        spKeyword strategyProvider @?= "rsi"
        spName strategyProvider @?= "RSI Mean Reversion"
        T.isPrefixOf "RSI-based" (spDescription strategyProvider) @?= True

    , testCase "Default parameters are valid" $ do
        let defaultParams = spDefaultParams strategyProvider
        spValidateParams strategyProvider defaultParams @?= True

    , testCase "Parameter parsing works with valid input" $ do
        let result = spParseParams strategyProvider ["14", "70.0", "30.0"]
        case result of
          Nothing -> assertFailure "Should parse valid RSI parameters"
          Just params -> spValidateParams strategyProvider params @?= True

    , testCase "Parameter parsing fails with invalid input" $ do
        let result = spParseParams strategyProvider ["invalid", "70.0", "30.0"]
        result @?= Nothing

    , testCase "Parameter validation rejects invalid ranges" $ do
        case spParseParams strategyProvider ["14", "30.0", "70.0"] of  -- Overbought < Oversold
          Nothing -> return ()  -- Expected failure
          Just params -> spValidateParams strategyProvider params @?= False
    ]

  , testGroup "RSI Signal Generation"
    [ testCase "RSI with insufficient data should hold" $ do
        let strategy = createTestRSIInstance
            initialState = siInitialState strategy
            candles = [createCandle 100.0]
            (signal, _) = siSignalGenerator strategy candles initialState
        signal @?= Hold

    , testCase "RSI signal generation with trending data" $ do
        let strategy = createTestRSIInstance
            -- Create a series of candles with upward trend to test overbought condition
            prices = [100.0, 101.0, 102.0, 103.0, 104.0, 105.0, 106.0, 107.0,
                     108.0, 109.0, 110.0, 111.0, 112.0, 113.0, 114.0, 115.0]
            candles = map createCandle prices
            initialState = siInitialState strategy

        -- Process candles sequentially to build up RSI state
        let processCandles [] state = state
            processCandles (c:cs) state =
              let (_, newState) = siSignalGenerator strategy [c] state
              in processCandles cs newState

            finalState = processCandles (init candles) initialState
            (finalSignal, _) = siSignalGenerator strategy [last candles] finalState

        -- With strong upward trend, RSI should eventually generate sell signal (overbought)
        assertBool "Should generate some signal with trending data" (finalSignal /= Hold)

    , testCase "RSI handles empty candle list" $ do
        let strategy = createTestRSIInstance
            initialState = siInitialState strategy
            (signal, newState) = siSignalGenerator strategy [] initialState
        signal @?= Hold
        newState @?= initialState
    ]

  , testGroup "RSI Parameter Validation"
    [ testCase "Valid RSI parameters pass validation" $ do
        let validCombinations =
              [ ["14", "70.0", "30.0"]  -- Standard RSI
              , ["21", "80.0", "20.0"]  -- Conservative levels
              , ["7", "75.0", "25.0"]   -- Shorter period
              ]
        mapM_ (\params -> do
          case spParseParams strategyProvider params of
            Nothing -> assertFailure $ "Should parse valid params: " ++ show params
            Just parsed -> spValidateParams strategyProvider parsed @?= True
          ) validCombinations

    , testCase "Invalid RSI parameters fail validation" $ do
        let invalidCombinations =
              [ ["0", "70.0", "30.0"]    -- Period too small
              , ["14", "30.0", "70.0"]   -- Overbought < Oversold
              , ["14", "101.0", "30.0"]  -- Overbought > 100
              , ["14", "70.0", "-5.0"]   -- Oversold < 0
              , ["14", "45.0", "30.0"]   -- Overbought < 50
              , ["14", "70.0", "55.0"]   -- Oversold > 50
              ]
        mapM_ (\params -> do
          case spParseParams strategyProvider params of
            Nothing -> return ()  -- Expected failure
            Just parsed -> spValidateParams strategyProvider parsed @?= False
          ) invalidCombinations

    , testCase "Empty parameter list uses defaults" $ do
        let result = spParseParams strategyProvider []
        case result of
          Nothing -> assertFailure "Should provide default parameters for empty input"
          Just params -> spValidateParams strategyProvider params @?= True
    ]
  ]
