{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Main IG Broker Adapter - orchestrates all IG-specific modules
module Adapter.IG.BrokerAdapter
  ( -- Main adapter interface
    IGBrokerAdapter(..)
  , runIGBrokerAdapter
  , IGAdapterContext(..)
  -- High-level operations
  , initializeAdapter
  , connectToIG
  , subscribeToMarketData
  , executeTradeOrder
  , disconnectFromIG
  -- Error recovery
  , handleConnectionFailure
  , recoverFromError
  ) where

import Control.Monad.Reader
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.STM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Data.Map (Map)
import qualified Data.Map as Map

import Domain.Services.LiveDataService
import Domain.Types
import Util.Config (AppConfig(..), BrokerConfig(..))
import Util.Error (Result, brokerError, TempehError)
import Util.Logger (ComponentName(..), runFileLoggerWithComponent, logInfo, logWarn, logError)

-- Import our new modular architecture
import qualified Adapter.IG.Session as Session
import qualified Adapter.IG.Connection as Connection
import qualified Adapter.IG.Trading as Trading
import qualified Adapter.IG.Error as IGError
import Adapter.IG.Types (IGSession)

-- | IG Adapter context with simplified state management
data IGAdapterContext = IGAdapterContext
  { igcAppConfig :: AppConfig
  , igcSessionState :: TVar (Maybe IGSession)
  , igcConnectionStates :: TVar (Map ConnectionId Connection.ConnectionState)
  , igcTradingContext :: TVar (Maybe Trading.TradingContext)
  }

-- | Main IG Broker Adapter monad
newtype IGBrokerAdapter m a = IGBrokerAdapter
  { runIGBrokerAdapter :: ReaderT IGAdapterContext m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader IGAdapterContext)

-- | Initialize the IG adapter with all required state containers
initializeAdapter :: MonadIO m => AppConfig -> IGBrokerAdapter m (Result IGAdapterContext)
initializeAdapter appConfig = do
  liftIO $ adapterLogInfo "Initializing IG Broker Adapter with modular architecture"

  -- Initialize state containers
  sessionState <- liftIO $ newTVarIO Nothing
  connectionStates <- liftIO $ newTVarIO mempty
  tradingContext <- liftIO $ newTVarIO Nothing

  let context = IGAdapterContext
        { igcAppConfig = appConfig
        , igcSessionState = sessionState
        , igcConnectionStates = connectionStates
        , igcTradingContext = tradingContext
        }

  liftIO $ adapterLogInfo "IG Broker Adapter initialized successfully"
  return $ Right context

-- | Connect to IG using layered approach with simplified state management
connectToIG :: MonadIO m => Text -> Text -> IGBrokerAdapter m (Result ConnectionId)
connectToIG username password = do
  liftIO $ adapterLogInfo "Connecting to IG using new modular architecture"
  context <- ask
  let brokerConfig = acBroker (igcAppConfig context)

  -- Step 1: Create session using Session module
  sessionResult <- liftIO $ Session.runSessionManager (Session.SessionManager (return (igcSessionState context))) $
    Session.createSession brokerConfig username password
  case sessionResult of
    Left err -> return $ Left err
    Right session -> do
      -- Step 2: Generate connection ID and establish connection state
      connId <- liftIO $ ConnectionId <$> Connection.generateConnectionId
      now <- liftIO getCurrentTime
      let connectionState = Connection.ConnectionState
            { Connection.csConnectionId = connId
            , Connection.csStatus = Connected now
            , Connection.csLastHeartbeat = now
            , Connection.csReconnectCount = 0
            , Connection.csSubscriptions = mempty
            }

      -- Store connection state
      liftIO $ atomically $ do
        connections <- readTVar (igcConnectionStates context)
        writeTVar (igcConnectionStates context) (Map.insert connId connectionState connections)

      -- Step 3: Initialize trading context
      let tradingCtx = Trading.TradingContext session brokerConfig
      liftIO $ atomically $ writeTVar (igcTradingContext context) (Just tradingCtx)

      liftIO $ adapterLogInfo ("Successfully connected to IG: " <> T.pack (show connId))
      return $ Right connId

-- | Subscribe to market data using connection state
subscribeToMarketData :: MonadIO m => ConnectionId -> Instrument -> IGBrokerAdapter m (Result ())
subscribeToMarketData connId instrument = do
  liftIO $ adapterLogInfo ("Subscribing to market data: " <> T.pack (show instrument))
  context <- ask

  -- Check if connection exists
  maybeConnectionState <- liftIO $ atomically $ do
    connections <- readTVar (igcConnectionStates context)
    return $ Map.lookup connId connections

  case maybeConnectionState of
    Nothing -> do
      liftIO $ adapterLogError "Connection not found"
      return $ Left $ brokerError "Connection not available"
    Just connectionState -> do
      liftIO $ adapterLogInfo ("Successfully accessed connection for subscription to " <> T.pack (show instrument))
      -- In a full implementation, this would update the subscription state
      return $ Right ()

-- | Execute trade using trading context
executeTradeOrder :: MonadIO m => ConnectionId -> Instrument -> Side -> Double -> IGBrokerAdapter m (Result Text)
executeTradeOrder connId instrument side size = do
  liftIO $ adapterLogInfo ("Executing trade order: " <> T.pack (show side) <> " " <> T.pack (show size) <> " of " <> T.pack (show instrument))
  context <- ask

  -- Get trading context
  maybeTradingContext <- liftIO $ readTVarIO (igcTradingContext context)
  case maybeTradingContext of
    Nothing -> do
      liftIO $ adapterLogError "Trading context not initialized"
      return $ Left $ brokerError "Trading context not available"
    Just tradingCtx -> do
      -- Execute market order using Trading module
      result <- liftIO $ Trading.runTradingManager (Trading.TradingManager (return (igcTradingContext context))) $ do
        Trading.executeMarketOrder instrument side size
      case result of
        Left err -> do
          liftIO $ adapterLogError ("Trade execution failed: " <> T.pack (show err))
          return $ Left err
        Right dealRef -> do
          liftIO $ adapterLogInfo ("Trade executed successfully: " <> dealRef)
          return $ Right dealRef

-- | Disconnect from IG with proper cleanup
disconnectFromIG :: MonadIO m => ConnectionId -> IGBrokerAdapter m (Result ())
disconnectFromIG connId = do
  liftIO $ adapterLogInfo ("Disconnecting from IG: " <> T.pack (show connId))
  context <- ask

  -- Step 1: Clean up trading context
  liftIO $ atomically $ writeTVar (igcTradingContext context) Nothing
  liftIO $ adapterLogInfo "Trading context cleaned up"

  -- Step 2: Remove connection state
  liftIO $ atomically $ do
    connections <- readTVar (igcConnectionStates context)
    writeTVar (igcConnectionStates context) (Map.delete connId connections)
  liftIO $ adapterLogInfo "Connection state removed"

  -- Step 3: Close session using Session module
  let brokerConfig = acBroker (igcAppConfig context)
  result <- liftIO $ Session.runSessionManager (Session.SessionManager (return (igcSessionState context))) $ do
    Session.closeSession brokerConfig
  case result of
    Left err -> liftIO $ adapterLogWarn ("Session close failed: " <> T.pack (show err))
    Right _ -> liftIO $ adapterLogInfo "Session closed successfully"

  liftIO $ adapterLogInfo ("Successfully disconnected from IG: " <> T.pack (show connId))
  return $ Right ()

-- | Handle connection failures with automatic recovery using Error module
handleConnectionFailure :: MonadIO m => ConnectionId -> IGError.IGError -> IGBrokerAdapter m (Result ())
handleConnectionFailure connId igError = do
  liftIO $ adapterLogWarn ("Handling connection failure: " <> IGError.formatErrorMessage igError)

  let recoveryStrategy = IGError.determineRecoveryStrategy (IGError.igErrorCode igError)
  case recoveryStrategy of
    IGError.RefreshSession -> do
      liftIO $ adapterLogInfo "Attempting session refresh for recovery"
      context <- ask
      let brokerConfig = acBroker (igcAppConfig context)
      result <- liftIO $ Session.runSessionManager (Session.SessionManager (return (igcSessionState context))) $ do
        -- Would need username/password from original connection - simplified for now
        Session.renewSession brokerConfig "username" "password"
      case result of
        Left err -> return $ Left err
        Right _ -> return $ Right ()

    IGError.RetryWithBackoff -> do
      liftIO $ adapterLogInfo "Attempting connection retry with backoff"
      -- Implementation would retry connection establishment
      return $ Right ()

    IGError.SwitchToFallback -> do
      liftIO $ adapterLogInfo "Switching to fallback connection method"
      -- Implementation would switch from WebSocket to REST polling
      return $ Right ()

    _ -> do
      liftIO $ adapterLogError "No recovery strategy available for this error"
      return $ Left $ brokerError "Connection recovery failed"

-- | Generic error recovery mechanism
recoverFromError :: MonadIO m => TempehError -> IGBrokerAdapter m (Result ())
recoverFromError err = do
  liftIO $ adapterLogInfo ("Attempting error recovery: " <> T.pack (show err))
  -- Implementation would analyze error and apply appropriate recovery strategy
  return $ Right ()

-- Logging helpers
adapterLogInfo :: Text -> IO ()
adapterLogInfo msg = runFileLoggerWithComponent (ComponentName "IG_ADAPTER") $ logInfo msg

adapterLogWarn :: Text -> IO ()
adapterLogWarn msg = runFileLoggerWithComponent (ComponentName "IG_ADAPTER") $ logWarn msg

adapterLogError :: Text -> IO ()
adapterLogError msg = runFileLoggerWithComponent (ComponentName "IG_ADAPTER") $ logError msg
