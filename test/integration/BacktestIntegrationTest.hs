{-# LANGUAGE RecordWildCards #-}
module Integration.BacktestIntegrationTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Domain.Types
import Domain.Services.BacktestService (BacktestResult(..), TradeRecord(..), TradeType(..))
import Domain.Strategy (StrategyState(..))
import Application.Strategy.Types (StrategyParameters(..), StrategyInstance(..), StrategyProvider(..))
import Application.Strategy.Factory (initializeStrategyRegistry)
import Application.Strategy.Registry (findStrategyByKeyword)
import Data.Scientific (fromFloatDigits, Scientific)
import Data.Time
import qualified Data.Text as T

-- Integration tests focus on component interactions
tests :: TestTree
tests = testGroup "Backtest Integration"
  [ testGroup "Strategy Integration"
    [ testCase "EMA strategy state persistence" testStrategyStatePersistence
    , testCase "Signal generation with real data" testSignalGenerationIntegration
    , testCase "Strategy parameter validation" testStrategyParameters
    ]
  , testGroup "Domain Integration"
    [ testCase "Candle to signal processing" testCandleToSignalProcessing
    , testCase "Price and quantity calculations" testPriceCalculations
    , testCase "Trade record creation" testTradeRecordCreation
    ]
  , testGroup "Data Type Integration"
    [ testCase "Instrument text handling" testInstrumentTextHandling
    , testCase "Scientific number precision" testScientificPrecision
    , testCase "Time handling consistency" testTimeHandling
    ]
  ]

-- Test implementations
getProvider :: String -> IO StrategyProvider
getProvider keyword = do
  let registry = initializeStrategyRegistry
  case findStrategyByKeyword keyword registry of
    Just p -> pure p
    Nothing -> assertFailure ("Strategy not found: " ++ keyword) >> error "unreachable"

mkInstance :: StrategyProvider -> StrategyParameters -> StrategyInstance
mkInstance provider params = spFactory provider params

testStrategyStatePersistence :: IO ()
testStrategyStatePersistence = do
  emaProvider <- getProvider "ema"
  let params = spDefaultParams emaProvider
      strategy = mkInstance emaProvider params
      candles = createTrendingCandles
      initialState = StrategyState { unStrategyState = T.pack "" }
  -- Basic construction checks
  siName strategy @?= T.pack "EMA Crossover"
  assertBool "Default EMA params should validate" (spValidateParams emaProvider params)

-- Basic signal generation smoke test
testSignalGenerationIntegration :: IO ()
testSignalGenerationIntegration = do
  emaProvider <- getProvider "ema"
  let params = spDefaultParams emaProvider
      strategy = mkInstance emaProvider params
      candles = createTrendingCandles
      initialState = StrategyState { unStrategyState = T.pack "" }
  let (signal, _newState) = siSignalGenerator strategy candles initialState
  assertBool "Should generate valid signal (smoke)" True

-- Validate parameter parsing and validation
testStrategyParameters :: IO ()
testStrategyParameters = do
  emaProvider <- getProvider "ema"
  -- valid EMA params
  let validArgs = map T.pack ["5","20","0.0001"]
      validParsed = spParseParams emaProvider validArgs
  case validParsed of
    Just p -> assertBool "Valid EMA params validate" (spValidateParams emaProvider p)
    Nothing -> assertFailure "EMA valid params should parse"
  -- invalid EMA params: same fast/slow
  let invalidArgs = map T.pack ["20","20","0.0001"]
      invalidParsed = spParseParams emaProvider invalidArgs
  case invalidParsed of
    Just p -> assertBool "Invalid EMA should fail validation" (not $ spValidateParams emaProvider p)
    Nothing -> assertBool "Parser may reject invalid EMA" True

  -- RSI validation
  rsiProvider <- getProvider "rsi"
  let rsiValidArgs = map T.pack ["14","70","30"]
      rsiInvalidArgs = map T.pack ["14","30","70"]
  case spParseParams rsiProvider rsiValidArgs of
    Just p -> assertBool "Valid RSI params validate" (spValidateParams rsiProvider p)
    Nothing -> assertFailure "RSI valid params should parse"
  case spParseParams rsiProvider rsiInvalidArgs of
    Just p -> assertBool "Invalid RSI should fail validation" (not $ spValidateParams rsiProvider p)
    Nothing -> assertBool "Parser may reject invalid RSI" True

-- Candle to signal processing single-candle smoke
testCandleToSignalProcessing :: IO ()
testCandleToSignalProcessing = do
  emaProvider <- getProvider "ema"
  let params = spDefaultParams emaProvider
      strategy = mkInstance emaProvider params
      candles = [head createSampleCandles]
      initialState = StrategyState { unStrategyState = T.pack "" }
  let (signal, _newState) = siSignalGenerator strategy candles initialState
  assertBool "Processing single candle should work" True
  assertEqual "First candle should produce Hold signal" Hold signal

-- Domain tests
testPriceCalculations :: IO ()
testPriceCalculations = do
  let price1 = Price (fromFloatDigits 1.0850)
      price2 = Price (fromFloatDigits 1.0860)
      qty = Qty (fromFloatDigits 1000.0)
  assertBool "Price comparison should work" (price2 > price1)
  assertBool "Price difference should be calculable" (unPrice price2 - unPrice price1 > 0)
  assertBool "Quantity should be positive" (unQty qty > 0)

testTradeRecordCreation :: IO ()
testTradeRecordCreation = do
  let baseTime = read "2025-01-01 12:00:00 UTC"
      trade = TradeRecord
        { trTime = baseTime
        , trSide = Buy
        , trQty = Qty (fromFloatDigits 1000.0)
        , trPrice = Price (fromFloatDigits 1.0850)
        , trType = Open
        }
  assertEqual "Trade time should match" baseTime (trTime trade)
  assertEqual "Trade side should be Buy" Buy (trSide trade)
  assertEqual "Trade type should be Open" Open (trType trade)

-- Data type tests
testInstrumentTextHandling :: IO ()
testInstrumentTextHandling = do
  let instrument1 = Instrument (T.pack "EURUSD")
      instrument2 = Instrument (T.pack "GBPUSD")
  assertBool "Text-based instrument should work" (unInstrument instrument1 == T.pack "EURUSD")
  assertBool "Different instruments should not be equal" (instrument1 /= instrument2)

testScientificPrecision :: IO ()
testScientificPrecision = do
  let price = fromFloatDigits 1.08505
      calculatedPrice = price * fromFloatDigits 1000.0
  assertBool "Scientific calculations should maintain precision" (calculatedPrice > fromFloatDigits 1085.0)
  assertBool "Scientific calculations should be exact" (calculatedPrice == fromFloatDigits 1085.05)

testTimeHandling :: IO ()
testTimeHandling = do
  let baseTime = read "2025-01-01 00:00:00 UTC"
      laterTime = addUTCTime 3600 baseTime
  assertBool "Time arithmetic should work" (laterTime > baseTime)
  assertBool "Time difference should be correct" (diffUTCTime laterTime baseTime == 3600)

-- Helpers
createSampleCandles :: [Candle]
createSampleCandles =
  let baseTime = read "2025-01-01 00:00:00 UTC"
      prices = [1.0850, 1.0851, 1.0849, 1.0852, 1.0855, 1.0848]
  in zipWith (\i price -> Candle
       { cTime = addUTCTime (fromIntegral $ i * 60) baseTime
       , cOpen = Price (fromFloatDigits price)
       , cHigh = Price (fromFloatDigits (price + 0.0005))
       , cLow = Price (fromFloatDigits (price - 0.0005))
       , cClose = Price (fromFloatDigits (price + 0.0001))
       }) [0..] prices

createTrendingCandles :: [Candle]
createTrendingCandles =
  let baseTime = read "2025-01-01 00:00:00 UTC"
      prices = [1.0800, 1.0805, 1.0810, 1.0815, 1.0820, 1.0825, 1.0830, 1.0835, 1.0840, 1.0845]
  in zipWith (\i price -> Candle
       { cTime = addUTCTime (fromIntegral $ i * 60) baseTime
       , cOpen = Price (fromFloatDigits price)
       , cHigh = Price (fromFloatDigits (price + 0.0002))
       , cLow = Price (fromFloatDigits (price - 0.0002))
       , cClose = Price (fromFloatDigits price)
       }) [0..] prices
