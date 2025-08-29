{-# LANGUAGE RecordWildCards #-}
module Adapter.SimBroker
  ( SimBroker
  , newSimBroker
  , feedTick
  , readOrders
  ) where

import Domain.Types
import Port.Broker
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.Scientific (Scientific)
import Data.Time (UTCTime)

data SimBroker = SimBroker
  { sbBalance :: TVar Scientific
  , sbOrders  :: TVar [(OrderId, Order)]
  , sbTicks   :: TChan Tick
  }

-- Create a new simulated broker with starting balance
newSimBroker :: Scientific -> IO SimBroker
newSimBroker bal = do
  balVar <- newTVarIO bal
  ordVar <- newTVarIO []
  tickCh <- newTChanIO
  pure (SimBroker balVar ordVar tickCh)

-- Allow pushing a tick into the sim broker's stream
feedTick :: SimBroker -> Tick -> IO ()
feedTick SimBroker{..} t = atomically $ writeTChan sbTicks t

-- Allow tests to read recorded orders
readOrders :: SimBroker -> IO [(OrderId, Order)]
readOrders SimBroker{..} = readTVarIO sbOrders

-- Broker instance for IO
instance Broker SimBroker IO where
  bGetAccount SimBroker{..} = do
    bal <- readTVarIO sbBalance
    pure (Account "SIM" bal)

  bMarketOrder SimBroker{..} o = atomically $ do
    ords <- readTVar sbOrders
    let oid = OrderId (length ords + 1)
    writeTVar sbOrders ((oid, o) : ords)
    pure oid

  bStreamTicks SimBroker{..} _instr = pure sbTicks
