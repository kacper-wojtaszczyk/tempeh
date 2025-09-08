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
  , convertIGMarketToTick
  , backoffDelay -- exported for tests
    -- Trading execution
  , executeEnterSignal
  , executeExitSignal
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
import Data.Maybe (isJust)
import Util.Logger (logInfo, logError, logWarn, logDebug, ComponentName(..), runFileLoggerWithComponent)

import Adapter.IG.Types
import Adapter.IG.Polling
import Adapter.IG.Auth
import Adapter.IG.Streaming
import Adapter.IG.Deals (createPosition, closePosition, getDealConfirmation)
import qualified Adapter.IG.Deals as IGDeals
import qualified Data.Scientific as Scientific
import Data.Scientific (Scientific)

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
              let ss = SubscriptionState tickBuffer now ticksRecv lastTick Nothing
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
            expected = max 0 (floor (elapsed * maxTPS))  -- maxTPS is now Double
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
                , bcMaxTicksPerSecond = max 0.1 (ltcMaxTicksPerSecond liveCfg)  -- Changed to work with Double and minimum 0.1/sec
                , bcStreamingMode = if isJust (igLightstreamerEndpoint session)
                                    then WebSocketStreaming
                                    else RESTPolling
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
        , bcMaxTicksPerSecond = max 0.1 (ltcMaxTicksPerSecond liveCfg)  -- Changed to work with Double
        , bcStreamingMode = RESTPolling  -- Demo mode uses REST polling
        }

  connectionsVar <- ask
  liftIO $ atomically $ do
    connections <- readTVar connectionsVar
    writeTVar connectionsVar (Map.insert connId connection connections)

  return $ Right connId

-- IG market data subscription with WebSocket streaming support
startIGMarketDataSubscription :: MonadIO m => BrokerConnection -> Instrument -> BrokerDataProviderM m ()
startIGMarketDataSubscription conn instrument = do
  let streamingMode = bcStreamingMode conn
  liftIO $ brokerLogInfo ("Starting IG market data subscription for " <> T.pack (show instrument) <> " using " <> T.pack (show streamingMode))

  maybeSession <- liftIO $ atomically $ readTVar (bcIGSession conn)
  case maybeSession of
    Nothing -> do
      liftIO $ brokerLogInfo "No IG session available for market data (running in demo/polling mode)"
      startRESTPolling conn instrument

    Just session ->
      case (bcStreamingMode conn, igLightstreamerEndpoint session) of
        (WebSocketStreaming, Just _endpoint) -> do
          liftIO $ brokerLogInfo "Starting Lightstreamer WebSocket streaming"
          startWebSocketStreaming conn instrument session

        _ -> do
          liftIO $ brokerLogInfo "No Lightstreamer endpoint or forced REST mode; using REST polling"
          startRESTPolling conn instrument

-- Start WebSocket streaming for an instrument
startWebSocketStreaming :: MonadIO m => BrokerConnection -> Instrument -> IGSession -> BrokerDataProviderM m ()
startWebSocketStreaming conn instrument session = do
  liftIO $ brokerLogInfo ("Establishing WebSocket streaming for " <> T.pack (show instrument))

  -- Get the tick buffer for this instrument
  subs <- liftIO $ atomically $ readTVar (bcSubscriptions conn)
  case Map.lookup instrument subs of
    Nothing -> do
      liftIO $ brokerLogError ("No subscription state found for instrument: " <> T.pack (show instrument))

    Just subscriptionState -> do
      -- Start WebSocket streaming in background
      _ <- liftIO $ async $ do
        streamingResult <- startLightstreamerConnection (bcConfig conn) session
        case streamingResult of
          Left err -> do
            brokerLogError ("Failed to start Lightstreamer connection: " <> T.pack (show err))
            brokerLogWarn "Falling back to REST polling"
            -- Fall back to REST polling on WebSocket failure
            igStreamingLoop conn instrument

          Right lsConnection -> do
            brokerLogInfo ("Lightstreamer connection established for " <> T.pack (show instrument))

            -- Subscribe to price updates
            subResult <- subscribeToPriceUpdates lsConnection instrument (ssTickBuffer subscriptionState)
            case subResult of
              Left err -> do
                brokerLogError ("Failed to subscribe to price updates: " <> T.pack (show err))
                brokerLogWarn "Falling back to REST polling"
                closeLightstreamerConnection lsConnection
                igStreamingLoop conn instrument

              Right _subscription -> do
                brokerLogInfo ("Successfully subscribed to WebSocket streaming for " <> T.pack (show instrument))

                -- Keep connection alive and handle reconnection
                handleWebSocketConnectionLifecycle lsConnection conn instrument

      liftIO $ brokerLogInfo ("Started WebSocket streaming setup for " <> T.pack (show instrument))

-- Handle WebSocket connection lifecycle (reconnection, error handling)
handleWebSocketConnectionLifecycle :: LSConnection -> BrokerConnection -> Instrument -> IO ()
handleWebSocketConnectionLifecycle lsConnection conn instrument = do
  brokerLogInfo "Starting WebSocket connection lifecycle management"

  -- Monitor connection and handle failures
  let monitorConnection = do
        brokerLogDebug "WebSocket connection monitoring active"
        threadDelay 5_000_000  -- Check every 5 seconds

        -- Check if connection is still alive
        connectionAlive <- checkWebSocketHealth lsConnection
        if connectionAlive
          then monitorConnection  -- Continue monitoring
          else do
            brokerLogWarn ("WebSocket connection lost for " <> T.pack (show instrument) <> ", attempting reconnection")

            -- Close existing connection
            closeLightstreamerConnection lsConnection

            -- Attempt to reconnect
            maybeSession <- atomically $ readTVar (bcIGSession conn)
            case maybeSession of
              Nothing -> do
                brokerLogError "No IG session available for reconnection, falling back to REST"
                igStreamingLoop conn instrument

              Just session -> do
                reconnectResult <- startLightstreamerConnection (bcConfig conn) session
                case reconnectResult of
                  Left err -> do
                    brokerLogError ("WebSocket reconnection failed: " <> T.pack (show err))
                    brokerLogWarn "Falling back to REST polling"
                    igStreamingLoop conn instrument

                  Right newLsConnection -> do
                    brokerLogInfo "WebSocket reconnection successful"

                    -- Re-subscribe to instrument
                    subs <- atomically $ readTVar (bcSubscriptions conn)
                    case Map.lookup instrument subs of
                      Just subscriptionState -> do
                        subResult <- subscribeToPriceUpdates newLsConnection instrument (ssTickBuffer subscriptionState)
                        case subResult of
                          Left _err -> do
                            brokerLogError "Failed to re-subscribe after reconnection, falling back to REST"
                            closeLightstreamerConnection newLsConnection
                            igStreamingLoop conn instrument

                          Right _subscription -> do
                            brokerLogInfo "Successfully re-subscribed after reconnection"
                            handleWebSocketConnectionLifecycle newLsConnection conn instrument

                      Nothing -> do
                        brokerLogError "Subscription state lost during reconnection"
                        closeLightstreamerConnection newLsConnection

  monitorConnection

-- Simple WebSocket health check
checkWebSocketHealth :: LSConnection -> IO Bool
checkWebSocketHealth _lsConnection = do
  -- For now, assume connection is healthy
  -- In a production system, you would send a ping and wait for pong
  return True

-- Start REST polling for an instrument (fallback method)
startRESTPolling :: MonadIO m => BrokerConnection -> Instrument -> BrokerDataProviderM m ()
startRESTPolling conn instrument = do
  liftIO $ brokerLogInfo ("Starting REST API polling for " <> T.pack (show instrument))
  -- Start background polling loop with delay
  _ <- liftIO $ async $ do
    threadDelay 200_000  -- Initial delay so flush is empty
    igStreamingLoop conn instrument
  liftIO $ brokerLogInfo ("Started REST polling for " <> T.pack (show instrument))

-- Helper to run broker data provider
runBrokerDataProviderIO :: BrokerDataProviderM IO a -> IO a
runBrokerDataProviderIO action = do
  connectionsVar <- newTVarIO Map.empty
  runReaderT (runBrokerDataProvider action) connectionsVar

-- Trading execution functions
executeEnterSignal :: (MonadIO m) => ConnectionId -> Instrument -> Domain.Types.Side -> Double -> BrokerDataProviderM m (Result Text)
executeEnterSignal connId instrument side positionSize = do
  liftIO $ brokerLogInfo ("Executing ENTER signal: " <> T.pack (show side) <> " " <> T.pack (show positionSize) <> " of " <> T.pack (show instrument))

  connectionsVar <- ask
  maybeConn <- liftIO $ atomically $ do
    connections <- readTVar connectionsVar
    return $ Map.lookup connId connections

  case maybeConn of
    Nothing -> do
      liftIO $ brokerLogError ("Connection not found: " <> T.pack (show connId))
      return $ Left $ brokerError ("Connection not found: " <> T.pack (show connId))

    Just conn -> do
      maybeSession <- liftIO $ atomically $ readTVar (bcIGSession conn)
      case maybeSession of
        Nothing -> do
          liftIO $ brokerLogWarn "No IG session available - running in demo mode, logging trade only"
          return $ Right "DEMO_TRADE_LOGGED"

        Just session -> do
          -- Create IG deal request
          let epic = instrumentToEpic instrument
              direction = sideToIGDirection side
              dealReq = IGDealRequest
                { dealEpic = epic
                , dealExpiry = "-" -- CFD (no expiry)
                , dealDirection = direction
                , dealSize = positionSize
                , dealOrderType = MARKET
                , dealLevel = Nothing
                , dealQuoteId = Nothing
                , dealCurrencyCode = Just "GBP"
                , dealForceOpen = Just True
                , dealGuaranteedStop = Nothing
                , dealStopLevel = Nothing
                , dealStopDistance = Nothing
                , dealTrailingStop = Nothing
                , dealTrailingStopIncrement = Nothing
                , dealLimitLevel = Nothing
                , dealLimitDistance = Nothing
                , dealTimeInForce = Nothing
                , dealReference = Just ("TEMPEH_" <> T.pack (show connId) <> "_" <> T.take 8 epic)
                }

          -- Execute the trade
          result <- liftIO $ createPosition (bcConfig conn) session dealReq
          case result of
            Left err -> do
              liftIO $ brokerLogError ("Failed to create position: " <> T.pack (show err))
              return $ Left err
            Right dealResponse -> do
              liftIO $ brokerLogInfo ("Position creation submitted: " <> dealResponseReference dealResponse)

              -- Wait a moment and check deal confirmation
              liftIO $ threadDelay 1000000 -- 1 second
              confirmResult <- liftIO $ getDealConfirmation (bcConfig conn) session (dealResponseReference dealResponse)
              case confirmResult of
                Left confErr -> do
                  liftIO $ brokerLogWarn ("Could not confirm deal: " <> T.pack (show confErr))
                  return $ Right $ dealResponseReference dealResponse
                Right confirmation -> do
                  liftIO $ brokerLogInfo ("Deal confirmed: " <> T.pack (show (confirmationDealStatus confirmation)))
                  return $ Right $ dealResponseReference dealResponse

executeExitSignal :: (MonadIO m) => ConnectionId -> Instrument -> BrokerDataProviderM m (Result Text)
executeExitSignal connId instrument = do
  liftIO $ brokerLogInfo ("Executing EXIT signal for " <> T.pack (show instrument))

  connectionsVar <- ask
  maybeConn <- liftIO $ atomically $ do
    connections <- readTVar connectionsVar
    return $ Map.lookup connId connections

  case maybeConn of
    Nothing -> do
      liftIO $ brokerLogError ("Connection not found: " <> T.pack (show connId))
      return $ Left $ brokerError ("Connection not found: " <> T.pack (show connId))

    Just conn -> do
      maybeSession <- liftIO $ atomically $ readTVar (bcIGSession conn)
      case maybeSession of
        Nothing -> do
          liftIO $ brokerLogWarn "No IG session available - running in demo mode, logging exit only"
          return $ Right "DEMO_EXIT_LOGGED"

        Just session -> do
          -- Get current positions for this instrument
          positionsResult <- liftIO $ IGDeals.getPositions (bcConfig conn) session
          case positionsResult of
            Left err -> do
              liftIO $ brokerLogError ("Failed to get positions: " <> T.pack (show err))
              return $ Left err
            Right positions -> do
              let epic = instrumentToEpic instrument
                  relevantPositions = filter (\pos -> positionEpic pos == epic) positions

              case relevantPositions of
                [] -> do
                  liftIO $ brokerLogWarn ("No open positions found for " <> epic)
                  return $ Right "NO_POSITIONS_TO_CLOSE"

                (position:_) -> do
                  -- Close the first matching position
                  let closeReq = IGDealRequest
                        { dealEpic = positionEpic position
                        , dealExpiry = "-"
                        , dealDirection = oppositeDirection (positionDirection position)
                        , dealSize = positionSize position
                        , dealOrderType = MARKET
                        , dealLevel = Nothing
                        , dealQuoteId = Nothing
                        , dealCurrencyCode = Just (positionCurrency position)
                        , dealForceOpen = Just False
                        , dealGuaranteedStop = Nothing
                        , dealStopLevel = Nothing
                        , dealStopDistance = Nothing
                        , dealTrailingStop = Nothing
                        , dealTrailingStopIncrement = Nothing
                        , dealLimitLevel = Nothing
                        , dealLimitDistance = Nothing
                        , dealTimeInForce = Nothing
                        , dealReference = Just ("TEMPEH_CLOSE_" <> positionDealId position)
                        }

                  result <- liftIO $ closePosition (bcConfig conn) session closeReq
                  case result of
                    Left err -> do
                      liftIO $ brokerLogError ("Failed to close position: " <> T.pack (show err))
                      return $ Left err
                    Right dealResponse -> do
                      liftIO $ brokerLogInfo ("Position close submitted: " <> dealResponseReference dealResponse)
                      return $ Right $ dealResponseReference dealResponse

-- Helper functions for converting between domain and IG types
instrumentToEpic :: Instrument -> Text
instrumentToEpic (Instrument instr) = case instr of
  "EURUSD" -> "CS.D.EURUSD.MINI.IP"
  "GBPUSD" -> "CS.D.GBPUSD.MINI.IP"
  "USDJPY" -> "CS.D.USDJPY.MINI.IP"
  "AUDUSD" -> "CS.D.AUDUSD.MINI.IP"
  "USDCHF" -> "CS.D.USDCHF.MINI.IP"
  "EURGBP" -> "CS.D.EURGBP.MINI.IP"
  "EURJPY" -> "CS.D.EURJPY.MINI.IP"
  _ -> instr  -- Fallback to original instrument name

sideToIGDirection :: Domain.Types.Side -> Direction
sideToIGDirection Domain.Types.Buy = BUY
sideToIGDirection Domain.Types.Sell = SELL

oppositeDirection :: Direction -> Direction
oppositeDirection BUY = SELL
oppositeDirection SELL = BUY
