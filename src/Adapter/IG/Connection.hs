{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Connection management module - handles broker connections and lifecycle
module Adapter.IG.Connection
  ( -- Connection operations
    ConnectionManager(..)
  , establishConnection
  , closeConnection
  , getConnectionState
  , reconnectWithBackoff
  , generateConnectionId
  -- Connection state
  , ConnectionState(..)
  , ConnectionHealth(..)
  ) where

import Control.Monad.Reader
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random (randomRIO)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.IORef (IORef, newIORef, atomicModifyIORef)
import System.IO.Unsafe (unsafePerformIO)

import Domain.Types (ConnectionId(..), ConnectionStatus(..), Instrument, LiveDataQuality(..))
import Adapter.IG.Types (BrokerConnection(..), SubscriptionState(..), StreamingMode(..), IGSession)
import Util.Config (BrokerConfig(..), ReconnectPolicy(..))
import Util.Error (Result, brokerError)
import Util.Logger (ComponentName(..), runFileLoggerWithComponent, logInfo, logWarn, logError, logDebug)

-- | Connection state management
data ConnectionState = ConnectionState
  { csConnectionId :: ConnectionId
  , csStatus :: ConnectionStatus
  , csLastHeartbeat :: UTCTime
  , csReconnectCount :: Int
  , csSubscriptions :: Map Instrument SubscriptionState
  }

-- | Connection health metrics
data ConnectionHealth = ConnectionHealth
  { chQuality :: LiveDataQuality
  , chLatency :: Maybe Double
  , chUptime :: Double
  , chReconnectCount :: Int
  } deriving (Show)

-- | Connection manager monad
newtype ConnectionManager m a = ConnectionManager
  { runConnectionManager :: ReaderT (TVar (Map ConnectionId BrokerConnection)) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (TVar (Map ConnectionId BrokerConnection)))

-- Core connection operations
establishConnection :: MonadIO m => BrokerConfig -> ConnectionId -> IGSession -> ConnectionManager m (Result ConnectionId)
establishConnection config connId session = do
  liftIO $ connLogInfo ("Establishing connection: " <> T.pack (show connId))
  connectionsVar <- ask
  now <- liftIO getCurrentTime

  -- Create connection state
  statusVar <- liftIO $ newTVarIO $ Connected now
  heartbeatVar <- liftIO $ newTVarIO now
  subsVar <- liftIO $ newTVarIO Map.empty
  reconnectVar <- liftIO $ newTVarIO 0
  sessionVar <- liftIO $ newTVarIO (Just session)

  let connection = BrokerConnection
        { bcConnectionId = connId
        , bcConfig = config
        , bcStatus = statusVar
        , bcLastHeartbeat = heartbeatVar
        , bcSubscriptions = subsVar
        , bcReconnectCount = reconnectVar
        , bcHeartbeatAsync = Nothing
        , bcIGSession = sessionVar
        , bcBufferSize = 1000 -- Default buffer size
        , bcMaxTicksPerSecond = 10.0 -- Default tick rate
        , bcStreamingMode = RESTPolling -- Default mode
        }

  -- Store connection
  liftIO $ atomically $ do
    connections <- readTVar connectionsVar
    writeTVar connectionsVar (Map.insert connId connection connections)

  liftIO $ connLogInfo ("Connection established: " <> T.pack (show connId))
  return $ Right connId

closeConnection :: MonadIO m => ConnectionId -> ConnectionManager m (Result ())
closeConnection connId = do
  liftIO $ connLogInfo ("Closing connection: " <> T.pack (show connId))
  connectionsVar <- ask

  result <- liftIO $ atomically $ do
    connections <- readTVar connectionsVar
    case Map.lookup connId connections of
      Nothing -> do
        -- Connection already closed or never existed - this is OK for idempotency
        return $ Right ()
      Just conn -> do
        writeTVar (bcStatus conn) Disconnected
        writeTVar connectionsVar (Map.delete connId connections)
        return $ Right ()

  -- Always log success since idempotent close is successful
  liftIO $ connLogInfo ("Connection closed: " <> T.pack (show connId))
  return result

getConnectionState :: MonadIO m => ConnectionId -> ConnectionManager m (Result ConnectionState)
getConnectionState connId = do
  connectionsVar <- ask
  liftIO $ atomically $ do
    connections <- readTVar connectionsVar
    case Map.lookup connId connections of
      Nothing -> return $ Left $ brokerError ("Connection not found: " <> T.pack (show connId))
      Just conn -> do
        status <- readTVar (bcStatus conn)
        heartbeat <- readTVar (bcLastHeartbeat conn)
        reconnectCount <- readTVar (bcReconnectCount conn)
        subs <- readTVar (bcSubscriptions conn)
        return $ Right $ ConnectionState connId status heartbeat reconnectCount subs

reconnectWithBackoff :: MonadIO m => ConnectionId -> ReconnectPolicy -> ConnectionManager m (Result ())
reconnectWithBackoff connId policy = do
  liftIO $ connLogInfo ("Attempting reconnection with backoff: " <> T.pack (show connId))

  -- Get current reconnect count
  connectionsVar <- ask
  maybeConn <- liftIO $ atomically $ do
    connections <- readTVar connectionsVar
    return $ Map.lookup connId connections

  case maybeConn of
    Nothing -> return $ Left $ brokerError ("Connection not found for reconnect: " <> T.pack (show connId))
    Just conn -> do
      reconnectCount <- liftIO $ readTVarIO (bcReconnectCount conn)

      if reconnectCount >= rpMaxRetries policy
        then do
          liftIO $ connLogError ("Max reconnection attempts reached: " <> T.pack (show connId))
          return $ Left $ brokerError "Max reconnection attempts reached"
        else do
          -- Calculate backoff delay
          let delay = calculateBackoffDelay policy reconnectCount
          liftIO $ connLogInfo ("Reconnecting in " <> T.pack (show delay) <> " seconds")
          liftIO $ threadDelay (round $ delay * 1_000_000)

          -- Increment reconnect count
          liftIO $ atomically $ modifyTVar (bcReconnectCount conn) (+1)

          -- Attempt reconnection (implementation depends on specific broker logic)
          liftIO $ connLogInfo ("Reconnection attempt completed: " <> T.pack (show connId))
          return $ Right ()

-- Global counter for unique connection IDs
{-# NOINLINE connectionCounter #-}
connectionCounter :: IORef Int
connectionCounter = unsafePerformIO (newIORef 1000000)

-- Helper function to generate connection IDs
generateConnectionId :: IO Text
generateConnectionId = do
  uniqueId <- atomicModifyIORef connectionCounter (\n -> (n + 1, n + 1))
  return $ T.pack (show uniqueId)

-- Helper functions
calculateBackoffDelay :: ReconnectPolicy -> Int -> Double
calculateBackoffDelay policy attempt =
  let initialDelaySeconds = fromRational $ toRational $ rpInitialDelay policy
      maxDelaySeconds = fromRational $ toRational $ rpMaxDelay policy
  in min maxDelaySeconds (initialDelaySeconds * (rpBackoffMultiplier policy ^ attempt))

-- Logging helpers
connLogInfo :: Text -> IO ()
connLogInfo msg = runFileLoggerWithComponent (ComponentName "IG_CONNECTION") $ logInfo msg

connLogWarn :: Text -> IO ()
connLogWarn msg = runFileLoggerWithComponent (ComponentName "IG_CONNECTION") $ logWarn msg

connLogError :: Text -> IO ()
connLogError msg = runFileLoggerWithComponent (ComponentName "IG_CONNECTION") $ logError msg

connLogDebug :: Text -> IO ()
connLogDebug msg = runFileLoggerWithComponent (ComponentName "IG_CONNECTION") $ logDebug msg
