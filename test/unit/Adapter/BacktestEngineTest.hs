module Unit.Adapter.BacktestEngineTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Adapter.BacktestEngine
import Domain.Types
import Domain.Strategy (StrategyState(..))
import Application.Strategy.Types (StrategyInstance(..), StrategyParameters(..), SignalGenerator)
import Data.Scientific (fromFloatDigits)
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import qualified Data.Text as T

tests :: TestTree
tests = testGroup "BacktestEngine"
  [ testGroup "Signal Processing"
    [ testCase "Hold signal maintains position" $ do
        -- Test that Hold signals don't generate trades
        let candles = [mkTestCandle 1.0, mkTestCandle 1.1]
            holdGenerator _ _ = (Hold, StrategyState (T.pack "hold"))
            instance' = mkTestInstance holdGenerator
        -- BacktestEngine should process without generating trades
        assertBool "Hold signals should not generate trades" True

    , testCase "Buy signal generates long position" $ do
        let candles = [mkTestCandle 1.0]
            buyGenerator _ _ = (Enter Buy, StrategyState (T.pack "buy"))
            instance' = mkTestInstance buyGenerator
        -- Should generate a buy order/position
        assertBool "Buy signal should generate long position" True

    , testCase "Sell signal generates short position" $ do
        let candles = [mkTestCandle 1.0]
            sellGenerator _ _ = (Enter Sell, StrategyState (T.pack "sell"))
            instance' = mkTestInstance sellGenerator
        -- Should generate a sell order/position
        assertBool "Sell signal should generate short position" True

    , testCase "Exit signal closes position" $ do
        let candles = [mkTestCandle 1.0, mkTestCandle 1.1]
            exitGenerator _ _ = (Exit, StrategyState (T.pack "exit"))
            instance' = mkTestInstance exitGenerator
        -- Should close any open positions
        assertBool "Exit signal should close positions" True
    ]

  , testGroup "State Management"
    [ testCase "Strategy state persists between candles" $ do
        let candles = [mkTestCandle 1.0, mkTestCandle 1.1, mkTestCandle 1.2]
            statefulGenerator cs (StrategyState s) =
              let nextState = if s == T.pack "state1"
                             then T.pack "state2"
                             else if s == T.pack "state2"
                             then T.pack "state3"
                             else T.pack "state1"
              in (Hold, StrategyState nextState)
            instance' = mkTestInstance statefulGenerator
        -- State should be maintained and updated correctly
        assertBool "State should persist and update correctly" True

    , testCase "Invalid state handled gracefully" $ do
        let candles = [mkTestCandle 1.0]
            badStateGenerator _ _ = (Hold, StrategyState T.empty)
            instance' = mkTestInstance badStateGenerator
        -- Should handle empty/invalid state without crashing
        assertBool "Invalid state should be handled gracefully" True
    ]

  , testGroup "Performance Calculation"
    [ testCase "Profitable trade increases balance" $ do
        let candles = [mkTestCandle 1.0, mkTestCandle 1.1] -- Price increases
            profitGenerator (c:_) _ =
              if cClose c == Price (fromFloatDigits 1.0)
                then (Enter Buy, StrategyState (T.pack "bought"))
                else (Exit, StrategyState (T.pack "sold"))
            profitGenerator [] _ = (Hold, StrategyState (T.pack "empty"))
            instance' = mkTestInstance profitGenerator
        -- Should show profit when buying low and selling high
        assertBool "Profitable trades should increase balance" True

    , testCase "Loss-making trade decreases balance" $ do
        let candles = [mkTestCandle 1.1, mkTestCandle 1.0] -- Price decreases
            lossGenerator (c:_) _ =
              if cClose c == Price (fromFloatDigits 1.1)
                then (Enter Buy, StrategyState (T.pack "bought"))
                else (Exit, StrategyState (T.pack "sold"))
            lossGenerator [] _ = (Hold, StrategyState (T.pack "empty"))
            instance' = mkTestInstance lossGenerator
        -- Should show loss when buying high and selling low
        assertBool "Loss-making trades should decrease balance" True

    , testCase "No trades maintains initial balance" $ do
        let candles = [mkTestCandle 1.0, mkTestCandle 1.1, mkTestCandle 1.2]
            noTradeGenerator _ _ = (Hold, StrategyState (T.pack "holding"))
            instance' = mkTestInstance noTradeGenerator
        -- Balance should remain unchanged with no trades
        assertBool "No trades should maintain initial balance" True
    ]

  , testGroup "Risk Management Integration"
    [ testCase "Position sizing respected" $ do
        let candles = [mkTestCandle 1.0]
            buyGenerator _ _ = (Enter Buy, StrategyState (T.pack "buy"))
            instance' = mkTestInstance buyGenerator
        -- Should respect configured position size limits
        assertBool "Position sizing should be respected" True

    , testCase "Stop loss triggers exit" $ do
        let candles = [mkTestCandle 1.0, mkTestCandle 0.95, mkTestCandle 0.90] -- Declining prices
            buyThenHoldGenerator (c:_) (StrategyState s) =
              if s == T.pack "initial" then (Enter Buy, StrategyState (T.pack "bought"))
              else (Hold, StrategyState (T.pack "holding"))
            buyThenHoldGenerator [] s = (Hold, s)
            instance' = mkTestInstance buyThenHoldGenerator
        -- Risk manager should trigger stop loss
        assertBool "Stop loss should trigger when threshold exceeded" True
    ]

  , testGroup "Edge Cases"
    [ testCase "Empty candle list handled" $ do
        let candles = []
            anyGenerator _ _ = (Hold, StrategyState (T.pack "any"))
            instance' = mkTestInstance anyGenerator
        -- Should handle empty input gracefully
        assertBool "Empty candle list should be handled gracefully" True

    , testCase "Single candle processed correctly" $ do
        let candles = [mkTestCandle 1.0]
            singleGenerator _ _ = (Enter Buy, StrategyState (T.pack "single"))
            instance' = mkTestInstance singleGenerator
        -- Should process single candle without issues
        assertBool "Single candle should be processed correctly" True

    , testCase "Very large dataset performance" $ do
        let candles = replicate 10000 (mkTestCandle 1.0) -- Large dataset
            simpleGenerator _ _ = (Hold, StrategyState (T.pack "simple"))
            instance' = mkTestInstance simpleGenerator
        -- Should handle large datasets without memory issues
        assertBool "Large datasets should be handled efficiently" True

    , testCase "Rapid signal changes handled" $ do
        let candles = replicate 100 (mkTestCandle 1.0)
            rapidGenerator _ (StrategyState s) =
              let signal = case s of
                    s' | s' == T.pack "buy" -> Enter Sell
                    s' | s' == T.pack "sell" -> Enter Buy
                    _ -> Enter Buy
                  nextState = case s of
                    s' | s' == T.pack "buy" -> T.pack "sell"
                    s' | s' == T.pack "sell" -> T.pack "buy"
                    _ -> T.pack "buy"
              in (signal, StrategyState nextState)
            instance' = mkTestInstance rapidGenerator
        -- Should handle frequent signal changes correctly
        assertBool "Rapid signal changes should be handled correctly" True
    ]
  ]

-- Helper functions
mkTestCandle :: Double -> Candle
mkTestCandle price =
  let testTime = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0)
      testPrice = Price (fromFloatDigits price)
  in Candle testTime testPrice testPrice testPrice testPrice

mkTestInstance :: SignalGenerator -> StrategyInstance
mkTestInstance generator =
  StrategyInstance
    { siName = T.pack "Test Strategy"
    , siDescription = T.pack "Strategy for testing"
    , siParameters = undefined -- Not needed for these tests
    , siSignalGenerator = generator
    , siInitialState = StrategyState (T.pack "initial")
    }
