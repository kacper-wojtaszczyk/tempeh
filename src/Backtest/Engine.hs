module Backtest.Engine
  ( BacktestConfig(..)
  , BacktestResult(..)
  , Trade(..)
  , TradeType(..)
  , runBacktest
  ) where

import Domain.Types
import Strategy.EmaCross (EmaState(..))
import Port.Strategy
import Data.Functor.Identity
import Data.Scientific (Scientific, fromFloatDigits)
import Data.Time (UTCTime)
import qualified Data.Text as T

-- Configuration for backtest parameters
data BacktestConfig = BacktestConfig
  { bcInitialBalance :: Scientific
  , bcPositionSize :: Scientific
  , bcInstrument :: Instrument
  } deriving (Show, Eq)

-- Trade record for tracking
data Trade = Trade
  { tradeTime :: UTCTime
  , tradeSide :: Side
  , tradeQty :: Qty
  , tradePrice :: Price
  , tradeType :: TradeType
  } deriving (Show, Eq)

data TradeType = Open | Close deriving (Show, Eq)

-- Result of backtest execution
data BacktestResult = BacktestResult
  { brFinalBalance :: Scientific
  , brTotalTrades :: Int
  , brPnL :: Scientific
  , brTrades :: [Trade]
  } deriving (Show, Eq)

-- Internal backtest state
data BacktestState = BacktestState
  { bsBalance :: Scientific
  , bsPosition :: Maybe Position
  , bsTrades :: [Trade]
  , bsStrategyState :: EmaState
  } deriving (Show)

-- Default backtest configuration
defaultConfig :: BacktestConfig
defaultConfig = BacktestConfig
  { bcInitialBalance = fromFloatDigits 10000.0
  , bcPositionSize = fromFloatDigits 1000.0
  , bcInstrument = Instrument (T.pack "EURUSD")
  }

-- Pure backtest execution function
runBacktest :: BacktestConfig -> Strategy EmaState Identity -> [Candle] -> BacktestResult
runBacktest config strategy candles =
  let initialState = BacktestState
        { bsBalance = bcInitialBalance config
        , bsPosition = Nothing
        , bsTrades = []
        , bsStrategyState = initState strategy
        }
      finalState = foldl (processCandle config strategy) initialState candles
  in BacktestResult
       { brFinalBalance = bsBalance finalState
       , brTotalTrades = length (bsTrades finalState)
       , brPnL = bsBalance finalState - bcInitialBalance config
       , brTrades = reverse (bsTrades finalState)
       }

-- Process a single candle through the strategy and update state with debugging
processCandle :: BacktestConfig -> Strategy EmaState Identity -> BacktestState -> Candle -> BacktestState
processCandle config strategy state candle =
  let Identity (newStrategyState, signal) = step strategy (bsStrategyState state) candle
      newState = processSignal config signal candle $ state { bsStrategyState = newStrategyState }
  in newState

-- Process trading signals with proper pure function syntax
processSignal :: BacktestConfig -> Signal -> Candle -> BacktestState -> BacktestState
processSignal _ Hold _ state = state
processSignal config Exit candle state = closePosition config candle state
processSignal config (Enter side) candle state =
  case bsPosition state of
    Nothing -> openPosition config side candle state
    Just pos -> if pSide pos == side
                then state -- Already in same direction
                else closeAndOpenPosition config side candle state

-- Open a new position
openPosition :: BacktestConfig -> Side -> Candle -> BacktestState -> BacktestState
openPosition config side candle state =
  let price = cClose candle -- Use close price for execution
      qty = Qty (bcPositionSize config)
      position = Position (bcInstrument config) side qty price
      trade = Trade (cTime candle) side qty price Open
  in state
     { bsPosition = Just position
     , bsTrades = trade : bsTrades state
     }

-- Close existing position
closePosition :: BacktestConfig -> Candle -> BacktestState -> BacktestState
closePosition config candle state =
  case bsPosition state of
    Nothing -> state
    Just pos ->
      let closePrice = cClose candle
          pnl = calculatePnL pos closePrice
          trade = Trade (cTime candle) (oppositeSide (pSide pos)) (pQty pos) closePrice Close
      in state
         { bsBalance = bsBalance state + pnl
         , bsPosition = Nothing
         , bsTrades = trade : bsTrades state
         }

-- Close position and immediately open new one
closeAndOpenPosition :: BacktestConfig -> Side -> Candle -> BacktestState -> BacktestState
closeAndOpenPosition config newSide candle state =
  let stateAfterClose = closePosition config candle state
  in openPosition config newSide candle stateAfterClose

-- Calculate P&L for a position
calculatePnL :: Position -> Price -> Scientific
calculatePnL pos exitPrice =
  let entryPrice = unPrice (pEntry pos)
      exit = unPrice exitPrice
      qty = unQty (pQty pos)
      rawPnl = case pSide pos of
        Buy -> (exit - entryPrice) * qty
        Sell -> (entryPrice - exit) * qty
  in rawPnl

-- Utility function
oppositeSide :: Side -> Side
oppositeSide Buy = Sell
oppositeSide Sell = Buy
