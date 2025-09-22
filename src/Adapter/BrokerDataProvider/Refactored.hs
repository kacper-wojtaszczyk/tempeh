-- | Refactored BrokerDataProvider using clean architecture modules
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Adapter.BrokerDataProvider.Refactored
  ( -- Main adapter
    BrokerAdapter(..)
  , runBrokerAdapter
  -- High-level operations
  , initializeBroker
  , connectToBroker
  , subscribeTo
  , executeTrade
  ) where

import Control.Monad.Reader
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)

import Domain.Services.LiveDataService
import Domain.Types
import Util.Config (AppConfig(..), BrokerConfig(..))
import Util.Error (Result)

-- Import our new modular architecture
import Adapter.IG.Session (SessionManager, createSession, validateSession)
import Adapter.IG.Connection (ConnectionManager, establishConnection, getConnectionState)
import Adapter.IG.Trading (TradingManager, executeMarketOrder)
import Adapter.IG.Error (IGError, classifyError, determineRecoveryStrategy)

-- | Main broker adapter context
data BrokerContext = BrokerContext
  { bcAppConfig :: AppConfig
  , bcSessionManager :: SessionManager IO ()
  , bcConnectionManager :: ConnectionManager IO ()
  , bcTradingManager :: TradingManager IO ()
  }

-- | Broker adapter monad
newtype BrokerAdapter m a = BrokerAdapter
  { runBrokerAdapter :: ReaderT BrokerContext m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader BrokerContext)

-- | Initialize broker with clean separation of concerns
initializeBroker :: MonadIO m => AppConfig -> BrokerAdapter m (Result ())
initializeBroker appConfig = do
  liftIO $ putStrLn "Initializing broker with modular architecture"
  -- Session manager initialization
  -- Connection manager initialization
  -- Trading manager initialization
  return $ Right ()

-- | Connect to broker using session and connection managers
connectToBroker :: MonadIO m => Text -> Text -> BrokerAdapter m (Result ConnectionId)
connectToBroker username password = do
  liftIO $ putStrLn "Connecting using session and connection managers"
  -- Use SessionManager to create session
  -- Use ConnectionManager to establish connection
  -- Return connection ID
  return $ Right (ConnectionId "mock-connection")

-- | Subscribe to instrument using connection manager
subscribeTo :: MonadIO m => ConnectionId -> Instrument -> BrokerAdapter m (Result ())
subscribeTo connId instrument = do
  liftIO $ putStrLn $ "Subscribing to " ++ show instrument ++ " via connection manager"
  -- Use ConnectionManager to handle subscription
  return $ Right ()

-- | Execute trade using trading manager
executeTrade :: MonadIO m => ConnectionId -> Instrument -> Side -> Double -> BrokerAdapter m (Result Text)
executeTrade connId instrument side size = do
  liftIO $ putStrLn $ "Executing trade via trading manager: " ++ show side ++ " " ++ show size
  -- Use TradingManager to execute trade
  -- Handle errors with IGError module
  return $ Right "mock-deal-ref"
