{-# LANGUAGE RecordWildCards #-}
module Integration.BacktestIntegrationTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Domain.Types
import Domain.Services.BacktestService
import Strategy.EmaCross
import Strategy.Config (StrategyParameters(..))
import Data.Scientific (fromFloatDigits, Scientific)
import Domain.Strategy
import Data.Functor.Identity
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
testStrategyStatePersistence :: IO ()
testStrategyStatePersistence = do
  let initialState = EmaState Nothing Nothing Nothing
      candles = createTrendingCandles -- Use trending candles for signal generation
      strategy = emaCrossStrategyWithConfig (Strategy.Config.EmaCrossParams 5 20 0.0001)

  -- Run strategy for a few steps
  let (finalState, signals) = runStrategySteps strategy initialState (take 10 candles)

  -- Verify state evolution
  assertBool "Final state should have EMA values" (isStateInitialized finalState)
  assertBool "Should generate some signals" (not $ null $ filter (/= Hold) signals)

testSignalGenerationIntegration :: IO ()
testSignalGenerationIntegration = do
  let strategy = emaCrossStrategyWithConfig (Strategy.Config.EmaCrossParams 5 20 0.0001)
      candles = createTrendingCandles  -- Create candles with clear trend

  let (_, signals) = runStrategySteps strategy (initState strategy) candles
      entrySignals = filter isEntrySignal signals

  assertBool "Should generate entry signals on trending data" (not $ null entrySignals)

testStrategyParameters :: IO ()
testStrategyParameters = do
  let params1 = Strategy.Config.EmaCrossParams 5 20 0.0001
      params2 = Strategy.Config.EmaCrossParams 10 30 0.0002
      strategy1 = emaCrossStrategyWithConfig params1
      strategy2 = emaCrossStrategyWithConfig params2
      candles = createTrendingCandles -- Use trending candles for parameter sensitivity

  let (_, signals1) = runStrategySteps strategy1 (initState strategy1) candles
      (_, signals2) = runStrategySteps strategy2 (initState strategy2) candles

  assertBool "Different parameters should produce different results" (signals1 /= signals2)

testCandleToSignalProcessing :: IO ()
testCandleToSignalProcessing = do
  let strategy = emaCrossStrategyWithConfig (Strategy.Config.EmaCrossParams 5 20 0.0001)
      singleCandle = head createSampleCandles
      initialState = initState strategy

  let (newState, signal) = runIdentity $ step strategy initialState singleCandle

  assertBool "Processing single candle should work" True
  assertEqual "First candle should produce Hold signal" Hold signal

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
      laterTime = addUTCTime 3600 baseTime  -- Add 1 hour

  assertBool "Time arithmetic should work" (laterTime > baseTime)
  assertBool "Time difference should be correct" (diffUTCTime laterTime baseTime == 3600)

-- Helper functions for creating test data
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
      -- Create uptrending prices for clear EMA crossover
      prices = [1.0800, 1.0805, 1.0810, 1.0815, 1.0820, 1.0825, 1.0830, 1.0835, 1.0840, 1.0845]
  in zipWith (\i price -> Candle
       { cTime = addUTCTime (fromIntegral $ i * 60) baseTime
       , cOpen = Price (fromFloatDigits price)
       , cHigh = Price (fromFloatDigits (price + 0.0002))
       , cLow = Price (fromFloatDigits (price - 0.0002))
       , cClose = Price (fromFloatDigits price)
       }) [0..] prices

-- Helper functions
runStrategySteps :: Strategy s Identity -> s -> [Candle] -> (s, [Signal])
runStrategySteps Strategy{..} initialState candles =
  foldl (\(state, signals) candle ->
    let (newState, signal) = runIdentity (step state candle)
    in (newState, signals ++ [signal])
  ) (initialState, []) candles

isStateInitialized :: EmaState -> Bool
isStateInitialized (EmaState (Just _) (Just _) (Just _)) = True
isStateInitialized _ = False

isEntrySignal :: Signal -> Bool
isEntrySignal (Enter _) = True
isEntrySignal _ = False
