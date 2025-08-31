module Backtest.Report
  ( BacktestReport(..)
  , generateReport
  , printReport
  ) where

import Backtest.Engine (BacktestResult(..), Trade(..), TradeType(..))
import Domain.Types (Price(..), Qty(..), Side(..), unPrice, unQty)
import Data.Scientific (Scientific, fromFloatDigits)

-- Comprehensive backtest report with proper metrics
data BacktestReport = BacktestReport
  { reportInitialBalance :: Scientific
  , reportFinalBalance :: Scientific
  , reportTotalPnL :: Scientific
  , reportTotalTrades :: Int
  , reportWinningTrades :: Int
  , reportLosingTrades :: Int
  , reportWinRate :: Scientific
  , reportAvgWin :: Scientific
  , reportAvgLoss :: Scientific
  , reportProfitFactor :: Scientific
  , reportMaxDrawdown :: Scientific
  , reportRecentTrades :: [Trade]
  } deriving (Show)

-- Generate a comprehensive report using Double for all division operations
generateReport :: Scientific -> BacktestResult -> BacktestReport
generateReport initialBalance result =
  let trades = brTrades result
      tradePairs = pairTrades trades
      pnlResults = map calculateTradeResult tradePairs
      (winPnls, lossPnls) = partition (> 0) pnlResults
      totalWins = length winPnls
      totalLosses = length lossPnls
      totalTrades = totalWins + totalLosses

      -- Convert to Double for all division operations to avoid Scientific precision issues
      winRate = if totalTrades > 0
                then fromIntegral totalWins * 100.0 / fromIntegral totalTrades
                else 0.0 :: Double

      avgWin = if totalWins > 0
               then realToFrac (sum winPnls) / fromIntegral totalWins
               else 0.0 :: Double
      avgLoss = if totalLosses > 0
                then realToFrac (abs (sum lossPnls)) / fromIntegral totalLosses
                else 0.0 :: Double

      profitFactor = if totalLosses > 0 && avgLoss > 0.0001
                     then realToFrac (sum winPnls) / realToFrac (abs (sum lossPnls))
                     else if totalWins > 0 then 999999.0 else 0.0 :: Double

      maxDD = realToFrac (calculateMaxDrawdown pnlResults) :: Double

  in BacktestReport
       { reportInitialBalance = initialBalance
       , reportFinalBalance = brFinalBalance result
       , reportTotalPnL = brPnL result
       , reportTotalTrades = brTotalTrades result
       , reportWinningTrades = totalWins
       , reportLosingTrades = totalLosses
       , reportWinRate = fromFloatDigits winRate
       , reportAvgWin = fromFloatDigits avgWin
       , reportAvgLoss = fromFloatDigits avgLoss
       , reportProfitFactor = fromFloatDigits profitFactor
       , reportMaxDrawdown = fromFloatDigits maxDD
       , reportRecentTrades = take 5 $ reverse trades
       }

-- Calculate maximum drawdown from P&L series
calculateMaxDrawdown :: [Scientific] -> Scientific
calculateMaxDrawdown pnls =
  let runningPnl = scanl1 (+) pnls
      peaks = scanl1 max runningPnl
      drawdowns = zipWith (-) peaks runningPnl
  in if null drawdowns then 0 else maximum drawdowns

-- Classify trades as winning or losing based on actual P&L
classifyTrades :: [Trade] -> (Int, Int)
classifyTrades trades =
  let tradePairs = pairTrades trades
      results = map calculateTradeResult tradePairs
      (wins, losses) = partition (> 0) results
  in (length wins, length losses)

-- Pair opening and closing trades
pairTrades :: [Trade] -> [(Trade, Trade)]
pairTrades [] = []
pairTrades [_] = []  -- Unpaired trade
pairTrades (open:close:rest)
  | tradeType open == Open && tradeType close == Close = (open, close) : pairTrades rest
  | otherwise = pairTrades (close:rest)  -- Skip malformed pairs

-- Calculate P&L for a trade pair
calculateTradeResult :: (Trade, Trade) -> Scientific
calculateTradeResult (openTrade, closeTrade) =
  let openPrice = unPrice (tradePrice openTrade)
      closePrice = unPrice (tradePrice closeTrade)
      qty = unQty (tradeQty openTrade)
  in case tradeSide openTrade of
       Buy  -> (closePrice - openPrice) * qty  -- Long position
       Sell -> (openPrice - closePrice) * qty  -- Short position

-- Helper function
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (filter p xs, filter (not . p) xs)

-- Print formatted report with proper Double arithmetic for percentages
printReport :: BacktestReport -> IO ()
printReport report = do
  putStrLn "\n=== BACKTEST RESULTS ==="
  putStrLn $ "Initial Balance: $" ++ show (reportInitialBalance report)
  putStrLn $ "Final Balance: $" ++ show (reportFinalBalance report)
  putStrLn $ "Total P&L: $" ++ show (reportTotalPnL report)

  -- Convert to Double BEFORE doing division to avoid Scientific precision issues
  let initialBal = realToFrac (reportInitialBalance report) :: Double
      finalBal = realToFrac (reportFinalBalance report) :: Double
      returnPct = if initialBal /= 0
                  then (finalBal - initialBal) / initialBal * 100
                  else 0.0
  putStrLn $ "Return: " ++ show returnPct ++ "%"
  putStrLn ""
  putStrLn "=== TRADING PERFORMANCE ==="
  putStrLn $ "Total Trades: " ++ show (reportTotalTrades report)
  putStrLn $ "Winning Trades: " ++ show (reportWinningTrades report)
  putStrLn $ "Losing Trades: " ++ show (reportLosingTrades report)
  putStrLn $ "Win Rate: " ++ show (realToFrac (reportWinRate report) :: Double) ++ "%"
  putStrLn $ "Average Win: $" ++ show (realToFrac (reportAvgWin report) :: Double)
  putStrLn $ "Average Loss: $" ++ show (realToFrac (reportAvgLoss report) :: Double)
  putStrLn $ "Profit Factor: " ++ show (realToFrac (reportProfitFactor report) :: Double)
  putStrLn $ "Max Drawdown: $" ++ show (realToFrac (reportMaxDrawdown report) :: Double)

  if reportTotalTrades report > 0
    then do
      putStrLn "\nLast 5 trades:"
      mapM_ print (reportRecentTrades report)
    else putStrLn "\nNo trades executed."
