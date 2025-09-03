{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}
module Adapter.BrokerDataProvider
  ( -- Types
    BrokerDataProviderM
  , IGMarket(..)
    -- Functions
  , runBrokerDataProviderIO
  , instrumentToIGEpic
  , convertIGMarketToTick
  , backoffDelay -- exported for tests
  ) where

import Domain.Services.LiveDataService
import Domain.Types
import Util.Config (BrokerConfig(..), BrokerType(..), BrokerEnvironment(..), ReconnectPolicy(..), AppConfig(..), acBroker, acLiveTrading, LiveTradingConfig(..), loadAppConfig)
import Util.Error (Result, TempehError(..), BrokerErrorDetails(..), brokerError)
import Control.Monad.Reader
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.STM
import Control.Concurrent.Async (async)
import Control.Concurrent (threadDelay)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.Map (Map)
import qualified Data.Map as Map
import Util.Logger (logInfo, logError, logWarn, logDebug, ComponentName(..), runFileLoggerWithComponent)

import Adapter.IG.Types
import Adapter.IG.Polling
import Adapter.IG.Auth

-- Adapter-scoped logging helpers
brokerLogInfo :: Text -> IO ()
brokerLogInfo msg = runFileLoggerWithComponent (ComponentName "BROKER") $ logInfo msg

brokerLogWarn :: Text -> IO ()
brokerLogWarn msg = runFileLoggerWithComponent (ComponentName "BROKER") $ logWarn msg

brokerLogError :: Text -> IO ()
brokerLogError msg = runFileLoggerWithComponent (ComponentName "BROKER") $ logError msg

brokerLogDebug :: Text -> IO ()
brokerLogDebug msg = runFileLoggerWithComponent (ComponentName "BROKER") $ logDebug msg

-- Broker adapter implementation
newtype BrokerDataProviderM m a = BrokerDataProviderM
  { runBrokerDataProvider :: ReaderT (TVar (Map ConnectionId BrokerConnection)) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (TVar (Map ConnectionId BrokerConnection)))

-- LiveDataProvider implementation for broker
instance (MonadIO m) => LiveDataProvider (BrokerDataProviderM m) where
  connect = do
    liftIO $ brokerLogInfo "Attempting to connect to broker"
    connId <- liftIO $ ConnectionId . T.pack . show <$> getCurrentTime
    now <- liftIO getCurrentTime

    -- Load configuration
    configResult <- liftIO $ loadAppConfig
    case configResult of
      Left configErr -> do
        liftIO $ brokerLogError ("Failed to load config: " <> configErr)
        return $ Left $ brokerError configErr
      Right appConfig -> do
        let brokerConfig = acBroker appConfig
        case bcBrokerType brokerConfig of
          IG   -> connectToIG appConfig connId brokerConfig now
          Demo -> connectDemo appConfig connId now
          _    -> do
            liftIO $ brokerLogWarn "Broker type not yet implemented, using demo mode"
            connectDemo appConfig connId now

  disconnect connId = do
    liftIO $ brokerLogInfo ("Disconnecting from broker: " <> T.pack (show connId))
    connectionsVar <- ask

    -- Get connection and perform logout if it's an IG connection
    maybeConn <- liftIO $ atomically $ do
      connections <- readTVar connectionsVar
      return $ Map.lookup connId connections

    case maybeConn of
      Nothing -> return $ Right ()
      Just conn -> do
        -- Logout from IG if we have a session
        maybeSession <- liftIO $ atomically $ readTVar (bcIGSession conn)
        case maybeSession of
          Just session -> do
            logoutResult <- liftIO $ logoutFromIG (bcConfig conn) session
            case logoutResult of
              Left err -> liftIO $ brokerLogError ("Logout failed: " <> T.pack (show err))
              Right _ -> liftIO $ brokerLogInfo "Successfully logged out from IG"
          Nothing -> return ()

        -- Update connection status and remove from registry
        liftIO $ atomically $ do
          connections <- readTVar connectionsVar
          case Map.lookup connId connections of
            Nothing -> return ()
            Just conn' -> do
              writeTVar (bcStatus conn') Disconnected
              writeTVar connectionsVar (Map.delete connId connections)

        return $ Right ()

  getConnectionStatus connId = do
    connectionsVar <- ask
    liftIO $ atomically $ do
      connections <- readTVar connectionsVar
      case Map.lookup connId connections of
        Nothing -> return $ Left $ brokerError ("Connection not found: " <> T.pack (show connId))
        Just conn -> Right <$> readTVar (bcStatus conn)

  subscribeToInstrument connId instrument = do
    liftIO $ brokerLogInfo ("Subscribing to instrument: " <> T.pack (show instrument))
    connectionsVar <- ask
    now <- liftIO getCurrentTime
    result <- liftIO $ atomically $ do
      connections <- readTVar connectionsVar
      case Map.lookup connId connections of
        Nothing -> return $ Left $ brokerError ("Connection not found: " <> T.pack (show connId))
        Just conn -> do
          subs <- readTVar (bcSubscriptions conn)
          if Map.member instrument subs
            then return $ Right conn
            else do
              tickBuffer <- newTVar []
              ticksRecv <- newTVar 0
              lastTick <- newTVar Nothing
              let ss = SubscriptionState tickBuffer now ticksRecv lastTick
              let newSubs = Map.insert instrument ss subs
              writeTVar (bcSubscriptions conn) newSubs
              return $ Right conn
    case result of
      Left err -> return $ Left err
      Right conn -> do
        -- Start IG market data subscription
        startIGMarketDataSubscription conn instrument
        return $ Right ()

  unsubscribeFromInstrument connId instrument = do
    liftIO $ brokerLogInfo ("Unsubscribing from instrument: " <> T.pack (show instrument))
    connectionsVar <- ask
    liftIO $ atomically $ do
      connections <- readTVar connectionsVar
      case Map.lookup connId connections of
        Nothing -> return $ Left $ brokerError ("Connection not found: " <> T.pack (show connId))
        Just conn -> do
          subs <- readTVar (bcSubscriptions conn)
          let newSubs = Map.delete instrument subs
          writeTVar (bcSubscriptions conn) newSubs
          return $ Right ()

  getTickStream connId instrument = do
    connectionsVar <- ask
    liftIO $ atomically $ do
      connections <- readTVar connectionsVar
      case Map.lookup connId connections of
        Nothing -> return $ Left $ brokerError ("Connection not found: " <> T.pack (show connId))
        Just conn -> do
          subs <- readTVar (bcSubscriptions conn)
          case Map.lookup instrument subs of
            Nothing -> return $ Left $ brokerError ("Not subscribed to instrument: " <> T.pack (show instrument))
            Just ss -> do
              let buf = ssTickBuffer ss
              let flush = do
                    xs <- readTVar buf
                    writeTVar buf []
                    return xs
              return $ Right flush

  getDataQuality connId instrument = do
    connectionsVar <- ask
    (liftIO $ atomically $ do
      connections <- readTVar connectionsVar
      case Map.lookup connId connections of
        Nothing -> return $ Left $ brokerError ("Connection not found: " <> T.pack (show connId))
        Just conn -> do
          subs <- readTVar (bcSubscriptions conn)
          case Map.lookup instrument subs of
            Nothing -> return $ Left $ brokerError ("Not subscribed to instrument: " <> T.pack (show instrument))
            Just ss -> do
              ticksRecv <- readTVar (ssTicksReceived ss)
              lastTk <- readTVar (ssLastTickTime ss)
              let start = ssStartTime ss
              return $ Right (ticksRecv, lastTk, start, bcMaxTicksPerSecond conn)
      ) >>= \case
      Left err -> return (Left err)
      Right (ticksRecv, lastTk, start, maxTPS) -> do
        now <- liftIO getCurrentTime
        let elapsed = realToFrac (now `diffUTCTime` start) :: Double
            expected = max 0 (floor (elapsed * fromIntegral (max 1 maxTPS)))
            latencyMs = fmap (\t -> realToFrac (realToFrac (now `diffUTCTime` t) * 1000.0) :: Double) lastTk
            scoreBase = if expected <= 0 then 1.0 else min 1.0 (fromIntegral ticksRecv / fromIntegral expected)
            score = max 0 (min 1 scoreBase)
        return $ Right LiveDataQuality
          { ldqTicksReceived = ticksRecv
          , ldqTicksExpected = expected
          , ldqLatency = latencyMs
          , ldqLastTickTime = lastTk
          , ldqQualityScore = score
          }

-- IG-specific connection logic
connectToIG :: MonadIO m => AppConfig -> ConnectionId -> BrokerConfig -> UTCTime -> BrokerDataProviderM m (Result ConnectionId)
connectToIG appCfg connId config now = do
  liftIO $ brokerLogInfo "Connecting to IG using REST API"

  case (bcUsername config, bcPassword config, bcApiKey config) of
    (Just username, Just password, Just _apiKey) -> do
      -- Attempt login to IG
      loginResult <- liftIO $ loginToIG config username password
      case loginResult of
        Left err -> do
          liftIO $ brokerLogError ("IG login failed: " <> T.pack (show err))
          return $ Left err
        Right session -> do
          liftIO $ brokerLogInfo "Successfully authenticated with IG"

          -- Create connection with IG session
          statusVar <- liftIO $ newTVarIO $ Connected now
          heartbeatVar <- liftIO $ newTVarIO now
          subsVar <- liftIO $ newTVarIO Map.empty
          reconnectVar <- liftIO $ newTVarIO 0
          sessionVar <- liftIO $ newTVarIO $ Just session
          let liveCfg = acLiveTrading appCfg

          let connection = BrokerConnection
                { bcConnectionId = connId
                , bcConfig = config
                , bcStatus = statusVar
                , bcLastHeartbeat = heartbeatVar
                , bcSubscriptions = subsVar
                , bcReconnectCount = reconnectVar
                , bcHeartbeatAsync = Nothing
                , bcIGSession = sessionVar
                , bcBufferSize = ltcTickBufferSize liveCfg
                , bcMaxTicksPerSecond = max 1 (ltcMaxTicksPerSecond liveCfg)
                }

          -- Store connection
          connectionsVar <- ask
          liftIO $ atomically $ do
            connections <- readTVar connectionsVar
            writeTVar connectionsVar (Map.insert connId connection connections)

          return $ Right connId
    _ -> do
      liftIO $ brokerLogWarn "Missing IG credentials, falling back to demo mode"
      connectDemo appCfg connId now

-- Demo connection (fallback)
connectDemo :: MonadIO m => AppConfig -> ConnectionId -> UTCTime -> BrokerDataProviderM m (Result ConnectionId)
connectDemo appCfg connId now = do
  liftIO $ brokerLogInfo "Creating demo connection"

  statusVar <- liftIO $ newTVarIO $ Connected now
  heartbeatVar <- liftIO $ newTVarIO now
  subsVar <- liftIO $ newTVarIO Map.empty
  reconnectVar <- liftIO $ newTVarIO 0
  sessionVar <- liftIO $ newTVarIO Nothing
  let liveCfg = acLiveTrading appCfg

  let mockConfig = BrokerConfig
        { bcBrokerType = Demo
        , bcEnvironment = DemoEnv
        , bcBaseUrl = Nothing
        , bcApiKey = Nothing
        , bcUsername = Nothing
        , bcPassword = Nothing
        , bcAccountId = Nothing
        , bcConnectTimeout = 30
        , bcReadTimeout = 60
        , bcReconnectPolicy = ReconnectPolicy 5 1.0 30.0 2.0
        }

  let connection = BrokerConnection
        { bcConnectionId = connId
        , bcConfig = mockConfig
        , bcStatus = statusVar
        , bcLastHeartbeat = heartbeatVar
        , bcSubscriptions = subsVar
        , bcReconnectCount = reconnectVar
        , bcHeartbeatAsync = Nothing
        , bcIGSession = sessionVar
        , bcBufferSize = ltcTickBufferSize liveCfg
        , bcMaxTicksPerSecond = max 1 (ltcMaxTicksPerSecond liveCfg)
        }

  connectionsVar <- ask
  liftIO $ atomically $ do
    connections <- readTVar connectionsVar
    writeTVar connectionsVar (Map.insert connId connection connections)

  return $ Right connId

-- IG market data subscription
startIGMarketDataSubscription :: MonadIO m => BrokerConnection -> Instrument -> BrokerDataProviderM m ()
startIGMarketDataSubscription conn instrument = do
  liftIO $ brokerLogInfo ("Starting IG market data subscription for " <> T.pack (show instrument))
  maybeSession <- liftIO $ atomically $ readTVar (bcIGSession conn)
  case maybeSession of
    Nothing -> liftIO $ brokerLogInfo "No IG session available for market data (running in demo/polling mode)"
    Just session ->
      case igLightstreamerEndpoint session of
        Just _ep -> liftIO $ brokerLogInfo "IG Lightstreamer endpoint present; streaming will be implemented here"
        Nothing -> liftIO $ brokerLogInfo "No Lightstreamer endpoint; falling back to REST polling"
  -- Start background loop (streaming or polling). Delay 200ms so initial flush is empty.
  _ <- liftIO $ async $ do
    threadDelay 200_000
    igStreamingLoop conn instrument
  liftIO $ brokerLogInfo ("Started market data polling for " <> T.pack (show instrument))

-- Helper to run broker data provider
runBrokerDataProviderIO :: BrokerDataProviderM IO a -> IO a
runBrokerDataProviderIO action = do
  connectionsVar <- newTVarIO Map.empty
  runReaderT (runBrokerDataProvider action) connectionsVar
