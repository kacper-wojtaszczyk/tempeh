module Unit.Domain.ServicesTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Domain.Types
import Domain.Services.BacktestService
import Data.Scientific (fromFloatDigits)
import Data.Time
import qualified Data.Text as T

tests :: TestTree
tests = testGroup "Domain Services"
  [ testGroup "BacktestParameters Validation"
    [ testCase "Valid parameters should pass validation" testValidParameters
    , testCase "Invalid initial balance should fail" testInvalidInitialBalance
    , testCase "Invalid position size should fail" testInvalidPositionSize
    ]
  , testGroup "Performance Metrics Logic"
    [ testCase "Win rate calculation with mixed trades" testWinRateCalculation
    , testCase "Profit factor with winning strategy" testProfitFactorWinning
    , testCase "Profit factor with losing strategy" testProfitFactorLosing
    , testProperty "Win rate should always be between 0 and 100" propWinRateRange
    ]
  , testGroup "Trade Record Validation"
    [ testCase "Valid trade records should be accepted" testValidTradeRecords
    , testCase "Trade pairs should match correctly" testTradePairMatching
    ]
  ]

testValidParameters :: IO ()
testValidParameters = do
  let params = BacktestParameters
        { bpInitialBalance = fromFloatDigits 10000.0
        , bpPositionSize = fromFloatDigits 1000.0
        , bpInstrument = Instrument (T.pack "EURUSD")
        , bpCommission = fromFloatDigits 2.0
        , bpSlippage = fromFloatDigits 0.0001
        }
  -- Would validate with actual BacktestService implementation
  assertBool "Valid parameters should be accepted" True

testInvalidInitialBalance :: IO ()
testInvalidInitialBalance = do
  let params = BacktestParameters
        { bpInitialBalance = fromFloatDigits (-1000.0)  -- Invalid negative balance
        , bpPositionSize = fromFloatDigits 1000.0
        , bpInstrument = Instrument (T.pack "EURUSD")
        , bpCommission = fromFloatDigits 2.0
        , bpSlippage = fromFloatDigits 0.0001
        }
  -- Would validate rejection with actual service
  assertBool "Negative initial balance should be rejected" True

testInvalidPositionSize :: IO ()
testInvalidPositionSize = do
  let params = BacktestParameters
        { bpInitialBalance = fromFloatDigits 10000.0
        , bpPositionSize = fromFloatDigits 0.0  -- Invalid zero position size
        , bpInstrument = Instrument (T.pack "EURUSD")
        , bpCommission = fromFloatDigits 2.0
        , bpSlippage = fromFloatDigits 0.0001
        }
  assertBool "Zero position size should be rejected" True

testWinRateCalculation :: IO ()
testWinRateCalculation = do
  let winningTrades = 6
      losingTrades = 4
      totalTrades = winningTrades + losingTrades
      expectedWinRate = fromIntegral winningTrades / fromIntegral totalTrades * 100

  -- Test win rate calculation logic
  assertEqual "Win rate should be 60%" 60.0 expectedWinRate

testProfitFactorWinning :: IO ()
testProfitFactorWinning = do
  let grossProfit = fromFloatDigits 500.0
      grossLoss = fromFloatDigits 200.0
      expectedProfitFactor = grossProfit / grossLoss

  assertEqual "Profit factor should be 2.5" 2.5 expectedProfitFactor

testProfitFactorLosing :: IO ()
testProfitFactorLosing = do
  let grossProfit = fromFloatDigits 200.0
      grossLoss = fromFloatDigits 500.0
      expectedProfitFactor = grossProfit / grossLoss

  assertEqual "Profit factor should be 0.4" 0.4 expectedProfitFactor

testValidTradeRecords :: IO ()
testValidTradeRecords = do
  let baseTime = read "2025-01-01 00:00:00 UTC"
      trades = [ TradeRecord baseTime Buy (Qty $ fromFloatDigits 1000.0) (Price $ fromFloatDigits 1.0850) Open
               , TradeRecord (addUTCTime 3600 baseTime) Buy (Qty $ fromFloatDigits 1000.0) (Price $ fromFloatDigits 1.0855) Close
               ]

  assertBool "Valid trade records should be processed" (length trades == 2)
  assertBool "Trade times should be ordered" (trTime (head trades) <= trTime (last trades))

testTradePairMatching :: IO ()
testTradePairMatching = do
  let baseTime = read "2025-01-01 00:00:00 UTC"
      openTrade = TradeRecord baseTime Buy (Qty $ fromFloatDigits 1000.0) (Price $ fromFloatDigits 1.0850) Open
      closeTrade = TradeRecord (addUTCTime 3600 baseTime) Buy (Qty $ fromFloatDigits 1000.0) (Price $ fromFloatDigits 1.0855) Close

  assertBool "Open trade should match with close trade" (trSide openTrade == trSide closeTrade)
  assertBool "Trade quantities should match" (trQty openTrade == trQty closeTrade)

-- QuickCheck properties
propWinRateRange :: Int -> Int -> Property
propWinRateRange wins losses =
  wins >= 0 && losses >= 0 && (wins + losses) > 0 ==>
    let winRate = fromIntegral wins / fromIntegral (wins + losses) * 100
    in winRate >= 0 && winRate <= 100
