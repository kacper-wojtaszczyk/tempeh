module Domain.Broker where

import Domain.Types

-- Abstract broker interface
class Monad m => Broker m where
  placeOrder  :: Order -> m ()
  getPosition :: m (Maybe Position)

-- A very simple in-memory broker for testing
data SimState = SimState
  { currentPos :: Maybe Position
  , orders     :: [Order]
  } deriving (Show, Eq)

newtype SimBroker a = SimBroker { runSim :: SimState -> (a, SimState) }

instance Functor SimBroker where
  fmap f (SimBroker g) = SimBroker $ \s -> let (a, s') = g s in (f a, s')

instance Applicative SimBroker where
  pure a = SimBroker $ \s -> (a, s)
  SimBroker f <*> SimBroker g = SimBroker $ \s ->
    let (fab, s1) = f s
        (a, s2)   = g s1
    in (fab a, s2)

instance Monad SimBroker where
  SimBroker g >>= f = SimBroker $ \s ->
    let (a, s1) = g s
        SimBroker h = f a
    in h s1

instance Broker SimBroker where
  placeOrder o = SimBroker $ \st -> ((), st { orders = o : orders st })
  getPosition  = SimBroker $ \st -> (currentPos st, st)
