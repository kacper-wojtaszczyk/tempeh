module Unit.Strategy.EmaCross.IndicatorsTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Test.Tasty.QuickCheck as QC
import Strategy.EmaCross (ema, computeEma, EmaState(..))
import Domain.Types (Price(..))
import Data.Scientific (fromFloatDigits)

tests :: TestTree
tests = testGroup "Indicators"
  [ testGroup "EMA Calculation"
    [ testCase "EMA of single element = element" $
        ema 5 [Price (fromFloatDigits 1.2345)]
          @?= [Price (fromFloatDigits 1.2345)]
    , testCase "EMA sequence should be smooth" testEmaSmoothing
    , testCase "EMA should react to price changes" testEmaReactivity
    , testCase "Shorter EMA should be more responsive" testEmaResponsiveness
    ]
  , testGroup "EMA State Management"
    [ testCase "Initial state should be empty" testInitialEmaState
    , testCase "State should accumulate values" testEmaStateAccumulation
    , testCase "State serialization should work" testEmaStateSerialization
    ]
  , testGroup "EMA Edge Cases"
    [ testCase "Empty price list should return empty" testEmaEmptyList
    , testCase "EMA with period 1 should equal prices" testEmaPeriodOne
    , testProperty "EMA should never exceed max/min of input" propEmaBounds
    ]
  ]

testEmaSmoothing :: IO ()
testEmaSmoothing = do
  let prices = map (Price . fromFloatDigits) [1.0, 2.0, 3.0, 4.0, 5.0]
      emaValues = ema 3 prices

  assertBool "EMA should smooth price movements" (length emaValues == length prices)
  -- First value should equal first price
  assertEqual "First EMA equals first price" (head prices) (head emaValues)

testEmaReactivity :: IO ()
testEmaReactivity = do
  let basePrices = replicate 10 (Price $ fromFloatDigits 1.0)
      spikePrice = Price $ fromFloatDigits 2.0
      pricesWithSpike = basePrices ++ [spikePrice]
      emaValues = ema 5 pricesWithSpike

  assertBool "EMA should react to price spike" (last emaValues > Price (fromFloatDigits 1.0))

testEmaResponsiveness :: IO ()
testEmaResponsiveness = do
  let prices = map (Price . fromFloatDigits) [1.0, 1.1, 1.2, 1.3, 1.4]
      shortEma = ema 2 prices
      longEma = ema 4 prices

  -- Short EMA should be closer to latest price
  let shortFinal = last shortEma
      longFinal = last longEma
      latestPrice = last prices

  assertBool "Short EMA should be more responsive" (abs (unPrice shortFinal - unPrice latestPrice) < abs (unPrice longFinal - unPrice latestPrice))

testInitialEmaState :: IO ()
testInitialEmaState = do
  let initialState = EmaState Nothing Nothing Nothing
  assertEqual "Initial fast EMA should be Nothing" Nothing (prevFast initialState)
  assertEqual "Initial slow EMA should be Nothing" Nothing (prevSlow initialState)
  assertEqual "Initial diff should be Nothing" Nothing (prevDiff initialState)

testEmaStateAccumulation :: IO ()
testEmaStateAccumulation = do
  let price1 = Price $ fromFloatDigits 1.0850
      price2 = Price $ fromFloatDigits 1.0855

      -- Compute first EMA
      ema1 = computeEma 5 Nothing price1
      state1 = EmaState ema1 Nothing Nothing

      -- Compute second EMA
      ema2 = computeEma 5 ema1 price2
      state2 = EmaState ema2 Nothing Nothing

  assertBool "First EMA should be Just price1" (ema1 == Just price1)
  assertBool "Second EMA should be different from price2" (ema2 /= Just price2)
  assertBool "State should accumulate EMA values" (prevFast state2 /= prevFast state1)

testEmaStateSerialization :: IO ()
testEmaStateSerialization = do
  let state = EmaState
        (Just $ Price $ fromFloatDigits 1.0850)
        (Just $ Price $ fromFloatDigits 1.0845)
        (Just $ fromFloatDigits 0.0005)
      serialized = show state
      deserialized = read serialized :: EmaState

  assertEqual "State should serialize and deserialize correctly" state deserialized

testEmaEmptyList :: IO ()
testEmaEmptyList = do
  let result = ema 5 []
  assertEqual "EMA of empty list should be empty" [] result

testEmaPeriodOne :: IO ()
testEmaPeriodOne = do
  let prices = map (Price . fromFloatDigits) [1.0, 2.0, 3.0]
      emaValues = ema 1 prices

  assertEqual "EMA with period 1 should equal input prices" prices emaValues

-- QuickCheck properties
propEmaBounds :: Property
propEmaBounds =
  QC.forAll genPrices $ \rawPrices ->
    let prices = map (Price . fromFloatDigits) rawPrices
        emaValues = ema 3 prices
        minPrice = minimum prices
        maxPrice = maximum prices
        epsilon = Price (fromFloatDigits 1e-6)
        geqWithEps a b = unPrice a >= unPrice b - unPrice epsilon
        leqWithEps a b = unPrice a <= unPrice b + unPrice epsilon
    in all (\ema -> geqWithEps ema minPrice && leqWithEps ema maxPrice) emaValues
  where
    genPrices :: QC.Gen [Double]
    genPrices = QC.listOf1 (QC.choose (0.8, 1.5))
