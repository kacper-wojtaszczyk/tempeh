{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module Adapter.BacktestEngine where

import Control.Monad.State
import Data.List (sortOn)
import Data.Scientific (Scientific, fromFloatDigits)
import Data.Time (UTCTime)
import qualified Data.Text as T
import Domain.Types
import Domain.Strategy (StrategyState(..))
import Domain.Services.BacktestService
import Application.Strategy.Types (StrategyInstance, SignalGenerator)
import Util.Error (Result, AppError(..))

-- Adapter implementation of BacktestService using State monad
data BacktestEngineState = BacktestEngineState
  { besBalance :: Scientific
  , besPosition :: Maybe Position
  , besTrades :: [TradeRecord]
  , besStrategyState :: StrategyState
  , besEquityCurve :: [(UTCTime, Scientific)]
  } deriving (Show)

newtype BacktestEngineM a = BacktestEngineM
  { runBacktestEngine :: StateT BacktestEngineState IO a }
  deriving (Functor, Applicative, Monad, MonadState BacktestEngineState, MonadIO)

instance BacktestService BacktestEngineM where
  executeBacktest params initialStrategyState signalGenerator candles = do
    let initialState = BacktestEngineState
          { besBalance = bpInitialBalance params
          , besPosition = Nothing
          , besTrades = []
          , besStrategyState = initialStrategyState
          , besEquityCurve = []
          }

    put initialState
    processCandlesWithSignalGenerator params signalGenerator candles

    finalState <- get
    pure $ Right $ BacktestResult
      { brFinalBalance = besBalance finalState
      , brTotalTrades = length (besTrades finalState)
      , brPnL = besBalance finalState - bpInitialBalance params
      , brTrades = reverse (besTrades finalState)
      , brFinalPosition = besPosition finalState
      }

  calculatePerformanceMetrics result initialBalance = do
    let trades = brTrades result
        tradePairs = pairTrades trades

        totalTrades = length tradePairs
        winningTrades = length $ filter (> 0) $ map calculateTradeResultPnL tradePairs

        winRate = if totalTrades > 0
                  then fromFloatDigits $ (fromIntegral winningTrades * 100.0) / fromIntegral totalTrades
                  else 0

        winPnls = filter (> 0) $ map calculateTradeResultPnL tradePairs
        lossPnls = filter (< 0) $ map calculateTradeResultPnL tradePairs

        avgWin = if length winPnls > 0
                 then fromFloatDigits $ realToFrac (sum winPnls) / fromIntegral (length winPnls)
                 else 0
        avgLoss = if length lossPnls > 0
                  then fromFloatDigits $ realToFrac (abs (sum lossPnls)) / fromIntegral (length lossPnls)
                  else 0

        profitFactor = if avgLoss > 0
                       then fromFloatDigits $ realToFrac (sum winPnls) / realToFrac (abs (sum lossPnls))
                       else 999999

        -- Realized equity curve based on closed trades
        sortedPairs = sortOn (trTime . snd) tradePairs
        equityPoints =
          let step (eq, acc) (o,c) =
                let pnl = calculateTradeResultPnL (o,c)
                    newEq = eq + pnl
                in (newEq, acc ++ [(trTime c, newEq)])
          in snd (foldl step (initialBalance, []) sortedPairs)
        -- Compute max drawdown from realized equity curve
        maxDrawdown =
          let eqVals = map snd equityPoints
              runMax = scanl1 max eqVals
              drawdowns = zipWith (-) runMax eqVals
          in if null drawdowns then 0 else maximum drawdowns

    pure $ Right $ PerformanceMetrics
      { pmWinRate = winRate
      , pmProfitFactor = profitFactor
      , pmMaxDrawdown = maxDrawdown
      , pmAverageWin = avgWin
      , pmAverageLoss = avgLoss
      , pmSharpeRatio = Nothing
      }

  validateBacktestParameters params =
    if bpInitialBalance params > 0 && bpPositionSize params > 0
      then pure $ Right ()
      else pure $ Left $ ValidationError "Invalid backtest parameters"

-- Core processing logic
processCandlesWithSignalGenerator :: BacktestParameters -> SignalGenerator -> [Candle] -> BacktestEngineM ()
processCandlesWithSignalGenerator params signalGenerator candles = do
  mapM_ (processCandleWithSignalGenerator params signalGenerator) candles
  -- Force-close any open position at end of backtest using last candle
  state <- get
  case (candles, besPosition state) of
    ([], _) -> pure ()
    (_, Nothing) -> pure ()
    (_, Just _) -> closeCurrentPosition (last candles)

processCandleWithSignalGenerator :: BacktestParameters -> SignalGenerator -> Candle -> BacktestEngineM ()
processCandleWithSignalGenerator params signalGenerator candle = do
  state <- get

  let (signal, newStrategyState) = signalGenerator [candle] (besStrategyState state)

  modify $ \s -> s { besStrategyState = newStrategyState }

  case signal of
    Enter side -> openPosition params side candle
    Exit -> closeCurrentPosition candle
    Hold -> pure ()  -- No position change
    _ -> pure ()  -- Handle other signal types

  -- Always record equity after processing the candle
  recordEquityPoint candle

-- Position management
openPosition :: BacktestParameters -> Side -> Candle -> BacktestEngineM ()
openPosition params side candle = do
  state <- get
  case besPosition state of
    Nothing -> do
      let price = cClose candle
          position = Position
            { pInstr = bpInstrument params
            , pSide = side
            , pQty = Qty (bpPositionSize params)
            , pEntry = price
            , pStopLoss = Nothing
            , pTakeProfit = Nothing
            }
          trade = TradeRecord
            { trTime = cTime candle
            , trSide = side
            , trQty = Qty (bpPositionSize params)
            , trPrice = price
            , trType = Open
            }

      put state
        { besPosition = Just position
        , besTrades = trade : besTrades state
        }
    Just _ -> pure ()  -- Already have position

closeCurrentPosition :: Candle -> BacktestEngineM ()
closeCurrentPosition candle = do
  state <- get
  case besPosition state of
    Nothing -> pure ()
    Just pos -> do
      let exitPrice = cClose candle
          pnl = calculatePositionPnL pos exitPrice
          closeTrade = TradeRecord
            { trTime = cTime candle
            , trSide = oppositeSide (pSide pos)
            , trQty = pQty pos
            , trPrice = exitPrice
            , trType = Close
            }

      put state
        { besBalance = besBalance state + pnl
        , besPosition = Nothing
        , besTrades = closeTrade : besTrades state
        }

recordEquityPoint :: Candle -> BacktestEngineM ()
recordEquityPoint candle = do
  state <- get
  let markToMarket = case besPosition state of
        Nothing -> besBalance state
        Just pos ->
          let upnl = calculatePositionPnL pos (cClose candle)
          in besBalance state + upnl
      newPoint = (cTime candle, markToMarket)
  put state { besEquityCurve = newPoint : besEquityCurve state }

-- Helper functions
calculatePositionPnL :: Position -> Price -> Scientific
calculatePositionPnL pos exitPrice =
  let entryPrice = unPrice (pEntry pos)
      exit = unPrice exitPrice
      qty = unQty (pQty pos)
  in case pSide pos of
       Buy -> (exit - entryPrice) * qty
       Sell -> (entryPrice - exit) * qty

oppositeSide :: Side -> Side
oppositeSide Buy = Sell
oppositeSide Sell = Buy

pairTrades :: [TradeRecord] -> [(TradeRecord, TradeRecord)]
pairTrades [] = []
pairTrades [_] = []
pairTrades (open:close:rest)
  | trType open == Open && trType close == Close = (open, close) : pairTrades rest
  | otherwise = pairTrades (close:rest)

calculateTradeResultPnL :: (TradeRecord, TradeRecord) -> Scientific
calculateTradeResultPnL (openTrade, closeTrade) =
  let openPrice = unPrice (trPrice openTrade)
      closePrice = unPrice (trPrice closeTrade)
      qty = unQty (trQty openTrade)
  in case trSide openTrade of
       Buy -> (closePrice - openPrice) * qty
       Sell -> (openPrice - closePrice) * qty
