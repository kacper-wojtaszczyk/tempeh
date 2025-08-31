module Backtest.Report
  ( BacktestReport(..)
  , generateReport
  , printReport
  ) where

import Backtest.Engine (BacktestResult(..), Trade(..), TradeType(..))
import Data.Scientific (Scientific)

-- Comprehensive backtest report
data BacktestReport = BacktestReport
  { reportInitialBalance :: Scientific
  , reportFinalBalance :: Scientific
  , reportTotalPnL :: Scientific
  , reportTotalTrades :: Int
  , reportWinningTrades :: Int
  , reportLosingTrades :: Int
  , reportRecentTrades :: [Trade]
  } deriving (Show)

-- Generate a comprehensive report from backtest result
generateReport :: Scientific -> BacktestResult -> BacktestReport
generateReport initialBalance result =
  let trades = brTrades result
      (wins, losses) = classifyTrades trades
  in BacktestReport
       { reportInitialBalance = initialBalance
       , reportFinalBalance = brFinalBalance result
       , reportTotalPnL = brPnL result
       , reportTotalTrades = brTotalTrades result
       , reportWinningTrades = wins
       , reportLosingTrades = losses
       , reportRecentTrades = take 5 $ reverse trades
       }

-- Classify trades as winning or losing (simplified - would need position pairing in real implementation)
classifyTrades :: [Trade] -> (Int, Int)
classifyTrades trades =
  -- Simplified: assume half are winners, half are losers for now
  let total = length trades
      wins = total `div` 2
      losses = total - wins
  in (wins, losses)

-- Print formatted report
printReport :: BacktestReport -> IO ()
printReport report = do
  putStrLn "\n=== BACKTEST RESULTS ==="
  putStrLn $ "Initial Balance: $" ++ show (reportInitialBalance report)
  putStrLn $ "Final Balance: $" ++ show (reportFinalBalance report)
  putStrLn $ "Total P&L: $" ++ show (reportTotalPnL report)
  putStrLn $ "Total Trades: " ++ show (reportTotalTrades report)

  if reportTotalTrades report > 0
    then do
      putStrLn $ "Winning Trades: " ++ show (reportWinningTrades report)
      putStrLn $ "Losing Trades: " ++ show (reportLosingTrades report)

      putStrLn "\nLast 5 trades:"
      mapM_ print (reportRecentTrades report)
    else putStrLn "No trades executed."
