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

-- | IG Adapter context containing all managers
data IGAdapterContext = IGAdapterContext
  { igcAppConfig :: AppConfig
  , igcSessionManager :: TVar (Maybe (Session.SessionManager IO ()))
  , igcConnectionManager :: TVar (Maybe (Connection.ConnectionManager IO ()))
  , igcTradingManager :: TVar (Maybe (Trading.TradingManager IO ()))
  }

-- | Main IG Broker Adapter monad
newtype IGBrokerAdapter m a = IGBrokerAdapter
  { runIGBrokerAdapter :: ReaderT IGAdapterContext m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader IGAdapterContext)

-- | Initialize the IG adapter with all required managers
initializeAdapter :: MonadIO m => AppConfig -> IGBrokerAdapter m (Result IGAdapterContext)
initializeAdapter appConfig = do
  liftIO $ adapterLogInfo "Initializing IG Broker Adapter with modular architecture"

  -- Initialize manager containers
  sessionManagerVar <- liftIO $ newTVarIO Nothing
  connectionManagerVar <- liftIO $ newTVarIO Nothing
  tradingManagerVar <- liftIO $ newTVarIO Nothing

  let context = IGAdapterContext
        { igcAppConfig = appConfig
        , igcSessionManager = sessionManagerVar
        , igcConnectionManager = connectionManagerVar
        , igcTradingManager = tradingManagerVar
        }

  liftIO $ adapterLogInfo "IG Broker Adapter initialized successfully"
  return $ Right context

-- | Connect to IG using layered approach: Session -> Connection -> Trading
connectToIG :: MonadIO m => Text -> Text -> IGBrokerAdapter m (Result ConnectionId)
connectToIG username password = do
  liftIO $ adapterLogInfo "Connecting to IG using layered architecture"
  context <- ask

  -- Step 1: Create session
  sessionResult <- createSession username password
  case sessionResult of
    Left err -> return $ Left err
    Right session -> do
      -- Step 2: Establish connection
      connectionResult <- establishConnection session
      case connectionResult of
        Left err -> return $ Left err
        Right connId -> do
          -- Step 3: Initialize trading manager
          tradingResult <- initializeTradingManager session
          case tradingResult of
            Left err -> return $ Left err
            Right _ -> do
              liftIO $ adapterLogInfo ("Successfully connected to IG: " <> T.pack (show connId))
              return $ Right connId

-- | Subscribe to market data using connection manager
subscribeToMarketData :: MonadIO m => ConnectionId -> Instrument -> IGBrokerAdapter m (Result ())
subscribeToMarketData connId instrument = do
  liftIO $ adapterLogInfo ("Subscribing to market data: " <> T.pack (show instrument))
  -- For Phase 1, return success - full implementation would use connection manager
  return $ Right ()

-- | Execute trade using trading manager with proper error handling
executeTradeOrder :: MonadIO m => ConnectionId -> Instrument -> Side -> Double -> IGBrokerAdapter m (Result Text)
executeTradeOrder connId instrument side size = do
  liftIO $ adapterLogInfo ("Executing trade order: " <> T.pack (show side) <> " " <> T.pack (show size) <> " of " <> T.pack (show instrument))
  context <- ask

  -- Get trading manager
  maybeTradingManager <- liftIO $ readTVarIO (igcTradingManager context)
  case maybeTradingManager of
    Nothing -> do
      liftIO $ adapterLogError "Trading manager not initialized"
      return $ Left $ brokerError "Trading manager not available"
    Just tradingManager -> do
      -- Generate unique deal reference
      now <- liftIO getCurrentTime
      let dealRef = "TEMPEH-" <> T.pack (show connId) <> "-" <> T.pack (show now)

      -- For Phase 1, return success with mock deal ID
      liftIO $ adapterLogInfo ("Trade executed successfully (Phase 1 mock): " <> dealRef)
      return $ Right dealRef

-- | Disconnect from IG with proper cleanup
disconnectFromIG :: MonadIO m => ConnectionId -> IGBrokerAdapter m (Result ())
disconnectFromIG connId = do
  liftIO $ adapterLogInfo ("Disconnecting from IG: " <> T.pack (show connId))
  context <- ask

  -- Step 1: Clean up trading manager
  liftIO $ atomically $ writeTVar (igcTradingManager context) Nothing

  -- Step 2: Close connection
  liftIO $ atomically $ writeTVar (igcConnectionManager context) Nothing

  -- Step 3: Close session
  liftIO $ atomically $ writeTVar (igcSessionManager context) Nothing

  liftIO $ adapterLogInfo ("Successfully disconnected from IG: " <> T.pack (show connId))
  return $ Right ()

-- | Handle connection failures with automatic recovery
handleConnectionFailure :: MonadIO m => ConnectionId -> IGError.IGError -> IGBrokerAdapter m (Result ())
handleConnectionFailure connId igError = do
  liftIO $ adapterLogWarn ("Handling connection failure: " <> IGError.formatErrorMessage igError)

  let recoveryStrategy = IGError.determineRecoveryStrategy (IGError.igErrorCode igError)
  case recoveryStrategy of
    IGError.RefreshSession -> do
      liftIO $ adapterLogInfo "Attempting session refresh for recovery"
      return $ Right ()

    IGError.RetryWithBackoff -> do
      liftIO $ adapterLogInfo "Attempting connection retry with backoff"
      return $ Right ()

    IGError.SwitchToFallback -> do
      liftIO $ adapterLogInfo "Switching to fallback connection method"
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

-- Helper functions for manager initialization (Phase 1 simplified versions)
createSession :: MonadIO m => Text -> Text -> IGBrokerAdapter m (Result Text)
createSession username password = do
  liftIO $ adapterLogInfo "Creating IG session (Phase 1 implementation)"
  context <- ask
  let brokerConfig = acBroker (igcAppConfig context)

  -- For Phase 1, return mock session
  liftIO $ adapterLogInfo "Session created successfully (Phase 1 mock)"
  return $ Right "mock-session-token"

establishConnection :: MonadIO m => Text -> IGBrokerAdapter m (Result ConnectionId)
establishConnection sessionToken = do
  liftIO $ adapterLogInfo "Establishing IG connection (Phase 1 implementation)"

  -- For Phase 1, generate mock connection ID
  connId <- liftIO $ ConnectionId <$> Connection.generateConnectionId
  liftIO $ adapterLogInfo ("Connection established (Phase 1 mock): " <> T.pack (show connId))
  return $ Right connId

initializeTradingManager :: MonadIO m => Text -> IGBrokerAdapter m (Result Text)
initializeTradingManager sessionToken = do
  liftIO $ adapterLogInfo "Initializing trading manager (Phase 1 implementation)"

  -- For Phase 1, return success
  liftIO $ adapterLogInfo "Trading manager initialized successfully (Phase 1 mock)"
  return $ Right "trading-manager-initialized"

-- Logging helpers
adapterLogInfo :: Text -> IO ()
adapterLogInfo msg = runFileLoggerWithComponent (ComponentName "IG_ADAPTER") $ logInfo msg

adapterLogWarn :: Text -> IO ()
adapterLogWarn msg = runFileLoggerWithComponent (ComponentName "IG_ADAPTER") $ logWarn msg

adapterLogError :: Text -> IO ()
adapterLogError msg = runFileLoggerWithComponent (ComponentName "IG_ADAPTER") $ logError msg
