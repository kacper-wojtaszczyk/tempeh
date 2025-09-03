{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Adapter.ReportGeneratorAdapter where

import Application.ReportingService
import Domain.Types
import Domain.Services.BacktestService
import Util.Error (Result, TempehError, unsupportedOperationError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import Data.Scientific (Scientific, fromFloatDigits)
import Data.Time (UTCTime, getCurrentTime, formatTime, defaultTimeLocale)
import Data.List (sortOn, groupBy, maximumBy, minimumBy)
import Data.Function (on)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE

-- Console-based report generator adapter
data ConsoleReportGenerator = ConsoleReportGenerator

newtype ConsoleReportGeneratorM a = ConsoleReportGeneratorM
  { runConsoleReportGenerator :: ReaderT ConsoleReportGenerator IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader ConsoleReportGenerator)

instance ReportGenerator ConsoleReportGeneratorM where
  generateReport result metrics rptCtx = do
    currentTime <- liftIO getCurrentTime

    let summary = generateReportSummary rptCtx result
        tradeAnalysis = analyzeTradeResults (brTrades result)
        equityCurve = calculateEquityCurve (rcInitialBalance rptCtx) result

        reportOutput = ReportOutput
          { roSummary = summary
          , roTradeAnalysis = tradeAnalysis
          , roPerformanceMetrics = metrics
          , roEquityCurve = equityCurve
          , roGeneratedAt = currentTime
          }

    -- Print the report to console
    liftIO $ printConsoleReport reportOutput
    pure $ Right reportOutput

  generateTradeReport trades = do
    let tradeText = formatTradesAsText trades
    pure $ Right tradeText

  generatePerformanceChart equityCurve = do
    let drawdownCurve = calculateDrawdownCurve equityCurve
        tradePoints = []  -- Simplified for now
        chartData = ChartData
          { cdEquityCurve = equityCurve
          , cdDrawdownCurve = drawdownCurve
          , cdTradePoints = tradePoints
          }
    pure $ Right chartData

  exportReport reportOutput format filePath = do
    case format of
      CSV -> do
        let csvContent = formatReportAsCSV reportOutput
        liftIO $ TIO.writeFile filePath csvContent
        pure $ Right ()
      JSON -> do
        let jsonContent = formatReportAsJSON reportOutput
        liftIO $ TIO.writeFile filePath jsonContent
        pure $ Right ()
      HTML -> do
        let htmlContent = formatReportAsHTML reportOutput
        liftIO $ TIO.writeFile filePath htmlContent
        pure $ Right ()
      PDF -> pure $ Left $ unsupportedOperationError "PDF export not implemented yet"

-- Implementation functions
generateReportSummary :: ReportContext -> BacktestResult -> ReportSummary
generateReportSummary ctx result =
  let initial = rcInitialBalance ctx
      finalB = brFinalBalance result
      totalReturn = if initial > 0 then (finalB - initial) / initial * 100 else 0
      months = monthsBetween (rcDateRange ctx)
      annualizedReturn = if months > 0 then totalReturn * fromIntegral (12 :: Int) / fromIntegral months else totalReturn
      drTxt = formatDateRange (rcDateRange ctx)
      stratTxt = T.pack (show (rcStrategy ctx))
  in ReportSummary
    { rsInstrument = rcInstrument ctx
    , rsDateRange = drTxt
    , rsStrategy = stratTxt
    , rsTotalReturn = totalReturn
    , rsAnnualizedReturn = annualizedReturn
    , rsFinalBalance = finalB
    }

monthsBetween :: DateRange -> Int
monthsBetween dr =
  let start = drStartYear dr * 12 + (drStartMonth dr - 1)
      end   = drEndYear dr * 12 + (drEndMonth dr - 1)
      diff  = end - start + 1
  in max 0 diff

analyzeTradeResults :: [TradeRecord] -> TradeAnalysis
analyzeTradeResults trades =
  let tradePairs = pairTrades trades
      pnls = map calculateTradePnL tradePairs
      winningTrades = filter (> 0) pnls
      losingTrades = filter (< 0) pnls

      largestWin = if null winningTrades then 0 else maximum winningTrades
      largestLoss = if null losingTrades then 0 else minimum losingTrades

      consecutiveWins = calculateConsecutiveWins pnls
      consecutiveLosses = calculateConsecutiveLosses pnls

  in TradeAnalysis
    { taTotalTrades = length tradePairs
    , taWinningTrades = length winningTrades
    , taLosingTrades = length losingTrades
    , taLargestWin = largestWin
    , taLargestLoss = largestLoss
    , taConsecutiveWins = consecutiveWins
    , taConsecutiveLosses = consecutiveLosses
    , taAverageTradeTime = calculateAverageTradeTime tradePairs
    }

calculateEquityCurve :: Scientific -> BacktestResult -> [(UTCTime, Scientific)]
calculateEquityCurve initial result =
  let pairs = pairTrades (brTrades result)
      sortedPairs = sortOn (trTime . snd) pairs
      step (eq, acc) (openT, closeT) =
        let pnl = calculateTradePnL (openT, closeT)
            newEq = eq + pnl
        in (newEq, acc ++ [(trTime closeT, newEq)])
      (_, equityPoints) = foldl step (initial, []) sortedPairs
  in equityPoints

calculateDrawdownCurve :: [(UTCTime, Scientific)] -> [(UTCTime, Scientific)]
calculateDrawdownCurve equityCurve =
  let runningMax = scanl1 max (map snd equityCurve)
      drawdowns = zipWith (\equity peak -> peak - equity) (map snd equityCurve) runningMax
  in zip (map fst equityCurve) drawdowns

-- Helper functions
pairTrades :: [TradeRecord] -> [(TradeRecord, TradeRecord)]
pairTrades [] = []
pairTrades [_] = []
pairTrades (open:close:rest)
  | trType open == Open && trType close == Close = (open, close) : pairTrades rest
  | otherwise = pairTrades (close:rest)

calculateTradePnL :: (TradeRecord, TradeRecord) -> Scientific
calculateTradePnL (openTrade, closeTrade) =
  let openPrice = unPrice (trPrice openTrade)
      closePrice = unPrice (trPrice closeTrade)
      qty = unQty (trQty openTrade)
  in case trSide openTrade of
       Buy -> (closePrice - openPrice) * qty
       Sell -> (openPrice - closePrice) * qty

calculateConsecutiveWins :: [Scientific] -> Int
calculateConsecutiveWins pnls =
  let winStreaks = map length $ filter (all (> 0)) $ groupByConsecutive (> 0) pnls
  in if null winStreaks then 0 else maximum winStreaks

calculateConsecutiveLosses :: [Scientific] -> Int
calculateConsecutiveLosses pnls =
  let lossStreaks = map length $ filter (all (< 0)) $ groupByConsecutive (< 0) pnls
  in if null lossStreaks then 0 else maximum lossStreaks

calculateAverageTradeTime :: [(TradeRecord, TradeRecord)] -> Scientific
calculateAverageTradeTime [] = 0
calculateAverageTradeTime tradePairs =
  let tradeDurations = map calculateTradeDuration tradePairs
      totalDuration = sum tradeDurations
  in totalDuration / fromIntegral (length tradePairs)

calculateTradeDuration :: (TradeRecord, TradeRecord) -> Scientific
calculateTradeDuration (open, close) =
  -- Simplified duration calculation in minutes
  fromFloatDigits 30.0  -- Placeholder

groupByConsecutive :: (a -> Bool) -> [a] -> [[a]]
groupByConsecutive _ [] = []
groupByConsecutive pred xs = groupBy (\a b -> pred a == pred b) xs

formatDateRange :: DateRange -> T.Text
formatDateRange dr = T.pack $ show (drStartYear dr) <> "-" <> show (drStartMonth dr) <>
                             " to " <> show (drEndYear dr) <> "-" <> show (drEndMonth dr)

formatTradesAsText :: [TradeRecord] -> T.Text
formatTradesAsText trades = T.unlines $ map formatTrade trades
  where
    formatTrade trade = T.concat
      [ T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (trTime trade)
      , " | ", T.pack $ show (trSide trade)
      , " | ", T.pack $ show (unQty (trQty trade))
      , " | ", T.pack $ show (unPrice (trPrice trade))
      , " | ", T.pack $ show (trType trade)
      ]

-- Console output functions
printConsoleReport :: ReportOutput -> IO ()
printConsoleReport report = do
  putStrLn "\n=== BACKTEST REPORT ==="
  putStrLn $ "Generated: " <> formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (roGeneratedAt report)
  putStrLn ""
  printReportSummary (roSummary report)
  putStrLn ""
  printTradeAnalysis (roTradeAnalysis report)
  putStrLn ""
  printPerformanceMetrics (roPerformanceMetrics report)

printReportSummary :: ReportSummary -> IO ()
printReportSummary summary = do
  putStrLn "=== SUMMARY ==="
  putStrLn $ "Instrument: " <> T.unpack (unInstrument (rsInstrument summary))
  putStrLn $ "Date Range: " <> T.unpack (rsDateRange summary)
  putStrLn $ "Strategy: " <> T.unpack (rsStrategy summary)
  putStrLn $ "Total Return: " <> show (rsTotalReturn summary) <> "%"
  putStrLn $ "Annualized Return: " <> show (rsAnnualizedReturn summary) <> "%"
  putStrLn $ "Final Balance: $" <> show (rsFinalBalance summary)

printTradeAnalysis :: TradeAnalysis -> IO ()
printTradeAnalysis analysis = do
  putStrLn "=== TRADE ANALYSIS ==="
  putStrLn $ "Trade Pairs: " <> show (taTotalTrades analysis)
  putStrLn $ "Winning Trades: " <> show (taWinningTrades analysis)
  putStrLn $ "Losing Trades: " <> show (taLosingTrades analysis)
  putStrLn $ "Largest Win: $" <> show (taLargestWin analysis)
  putStrLn $ "Largest Loss: $" <> show (taLargestLoss analysis)
  putStrLn $ "Max Consecutive Wins: " <> show (taConsecutiveWins analysis)
  putStrLn $ "Max Consecutive Losses: " <> show (taConsecutiveLosses analysis)

printPerformanceMetrics :: PerformanceMetrics -> IO ()
printPerformanceMetrics metrics = do
  putStrLn "=== PERFORMANCE METRICS ==="
  putStrLn $ "Win Rate: " <> show (pmWinRate metrics) <> "%"
  putStrLn $ "Profit Factor: " <> show (pmProfitFactor metrics)
  putStrLn $ "Average Win: $" <> show (pmAverageWin metrics)
  putStrLn $ "Average Loss: $" <> show (pmAverageLoss metrics)
  putStrLn $ "Max Drawdown: $" <> show (pmMaxDrawdown metrics)

-- Placeholder formatting functions
formatReportAsCSV :: ReportOutput -> T.Text
formatReportAsCSV _ = "CSV format not implemented yet"

formatReportAsJSON :: ReportOutput -> T.Text
formatReportAsJSON ro = TE.decodeUtf8 . LBS.toStrict $ JSON.encode ro

formatReportAsHTML :: ReportOutput -> T.Text
formatReportAsHTML _ = "HTML format not implemented yet"
