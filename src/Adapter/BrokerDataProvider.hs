{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}

-- | Broker Data Provider - Refactored to use modular IG adapter architecture
-- This module implements the LiveDataProvider interface using the new modular IG components
module Adapter.BrokerDataProvider
  ( -- Types
    BrokerDataProviderM
    -- Functions
  , runBrokerDataProviderIO
    -- LiveDataProvider interface
  , connect
  , disconnect
  , getConnectionStatus
  , subscribeToInstrument
  , unsubscribeFromInstrument
  , getTickStream
  , getDataQuality
    -- Trading execution
  , executeEnterSignal
  , executeExitSignal
  , mkDealReference -- exported for tests
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
import Data.Time (getCurrentTime, diffUTCTime, utctDay)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (toGregorian, fromGregorian)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Scientific as Scientific
import Data.Scientific (Scientific)
import Data.Char (isAlphaNum, intToDigit, chr, ord)
import Numeric (showIntAtBase)
import Data.Bits ((.|.), shiftL)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, fromMaybe)
import System.IO.Unsafe (unsafePerformIO)
import Util.Logger (ComponentLogger, makeComponentLogger)

-- Import our new modular architecture (Phase 4: Cleaned up from legacy patterns)
import qualified Adapter.IG.Session as Session
import qualified Adapter.IG.Connection as Connection
import qualified Adapter.IG.Trading as Trading
import qualified Adapter.IG.Error as IGError
import qualified Adapter.IG.Deals as IGDeals
-- Keep existing imports that are still needed
import Adapter.IG.Types
import qualified Adapter.IG.Polling as IGPolling
import qualified Adapter.IG.Streaming as IGStreaming
import qualified Adapter.IG.Auth as IGAuth

-- Component logger for this module
brokerLogger :: ComponentLogger
brokerLogger = makeComponentLogger "BROKER"

-- | Broker data provider context - simplified for Phase 2+3 implementation
data BrokerContext = BrokerContext
  { bcAppConfig :: AppConfig
  , bcConnections :: TVar (Map ConnectionId BrokerConnection)
  }

-- Broker adapter implementation using legacy types during transition
newtype BrokerDataProviderM m a = BrokerDataProviderM
  { runBrokerDataProvider :: ReaderT BrokerContext m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader BrokerContext)

-- | Generate a 7-character base36 connection ID unique within a month
genShortConnId :: IO Text
genShortConnId = do
  now <- getCurrentTime
  let (year, month, _) = toGregorian (utctDay now)
      monthStart = fromGregorian year month 1
      secondsSinceMonthStart = round $ diffUTCTime now (UTCTime monthStart 0)
  -- Use POSIX time for pseudo-randomness instead of System.Random
  posixTime <- getPOSIXTime
  let randBits = (round (posixTime * 1000000) :: Int) `mod` 4096  -- 12 bits from microseconds
      rawNum = (secondsSinceMonthStart `shiftL` 12) .|. randBits
      base36 = showBase36 rawNum
      padded = T.justifyRight 7 '0' (T.pack base36)
  return $ T.takeEnd 7 padded
  where
    showBase36 :: Int -> String
    showBase36 n = showIntAtBase 36 toBase36Digit n ""

    toBase36Digit :: Int -> Char
    toBase36Digit i
      | i < 10    = intToDigit i
      | i < 36    = chr (ord 'a' + i - 10)
      | otherwise = error "Invalid digit for base36"

-- LiveDataProvider implementation with improved modular approach
instance (MonadIO m) => LiveDataProvider (BrokerDataProviderM m) where
  connect = do
    ctx <- ask
    case bdpBrokerType ctx of
      IGBroker -> do
        liftIO $ compLogInfo brokerLogger "Connecting to broker (Phase 2+3 modular integration)"

        -- Get IG credentials from config safely
        let config = bdpAppConfig ctx
            brokerConfig = acBroker config

        case (bcUsername brokerConfig, bcPassword brokerConfig) of
          (Just username, Just password) -> do
            -- Use the proper IG Adapter interface with enhanced modular architecture
            result <- liftIO $ connectToIGWithModularArchitecture config username password
            case result of
              Left err -> do
                liftIO $ compLogWarn brokerLogger "Broker type not implemented, using demo mode"
                return $ Right DemoConnectionId
              Right connId -> return $ Right connId
          _ -> do
            liftIO $ compLogWarn brokerLogger "Broker type not implemented, using demo mode"
            return $ Right DemoConnectionId

  disconnect connId = do
    liftIO $ compLogInfo brokerLogger ("Disconnecting from broker: " <> T.pack (show connId))

    ctx <- ask
    case bdpBrokerType ctx of
      IGBroker -> case connId of
        IGConnectionId igConnId -> do
          let config = bdpAppConfig ctx

          -- Enhanced disconnection with proper session cleanup
          case (acBroker config) of
            brokerConfig -> do
              -- Check if we have active session to logout
              case (bcUsername brokerConfig, bcPassword brokerConfig) of
                (Just _, Just _) -> do
                  -- Proper logout using Auth module
                  result <- liftIO $ logoutWithProperSessionCleanup config
                  case result of
                    Left err -> liftIO $ compLogError brokerLogger ("Logout failed: " <> T.pack (show err))
                    Right _ -> liftIO $ compLogInfo brokerLogger "Successfully logged out from IG"
                  return $ Right ()
                _ -> return $ Right ()

          return $ Right ()
        DemoConnectionId -> return $ Right ()
      _ -> return $ Right ()

  getConnectionStatus connId = do
    context <- ask
    let connectionsVar = bcConnections context
    liftIO $ atomically $ do
      connections <- readTVar connectionsVar
      case Map.lookup connId connections of
        Nothing -> return $ Left $ brokerError ("Connection not found: " <> T.pack (show connId))
        Just conn -> Right <$> readTVar (bcStatus conn)

  subscribeToInstrument connId instrument = do
    liftIO $ compLogInfo brokerLogger ("Subscribing to instrument: " <> T.pack (show instrument))

    ctx <- ask
    case bdpBrokerType ctx of
      IGBroker -> case connId of
        IGConnectionId igConnId -> do
          -- Use new IG modular architecture for subscription
          let config = bdpAppConfig ctx
          result <- liftIO $ subscribeToIGInstrument config igConnId instrument
          return result
        DemoConnectionId -> do
          -- Demo subscription (always succeeds)
          return $ Right ()
      _ -> return $ Right ()

  unsubscribeFromInstrument connId instrument = do
    liftIO $ compLogInfo brokerLogger ("Unsubscribing from instrument: " <> T.pack (show instrument))

    ctx <- ask
    case bdpBrokerType ctx of
      IGBroker -> case connId of
        IGConnectionId igConnId -> do
          -- Use new IG modular architecture for unsubscription
          let config = bdpAppConfig ctx
          result <- liftIO $ unsubscribeFromIGInstrument config igConnId instrument
          return result
        DemoConnectionId -> return $ Right ()
      _ -> return $ Right ()

  getTickStream connId instrument = do
    context <- ask
    let connectionsVar = bcConnections context
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
    context <- ask
    let connectionsVar = bcConnections context
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

-- IG-specific connection logic with enhanced modular integration
connectToIGModular :: MonadIO m => ConnectionId -> BrokerConfig -> UTCTime -> AppConfig -> BrokerDataProviderM m (Result ConnectionId)
connectToIGModular connId config now appConfig = do
  liftIO $ compLogInfo brokerLogger "Connecting to IG with enhanced modular architecture"

  case (bcUsername config, bcPassword config, bcApiKey config) of
    (Just username, Just password, Just _apiKey) -> do
      -- Attempt login to IG using qualified auth module
      loginResult <- liftIO $ IGAuth.loginToIG config username password
      case loginResult of
        Left err -> do
          liftIO $ compLogError brokerLogger ("IG login failed: " <> T.pack (show err))
          return $ Left err
        Right session -> do
          liftIO $ compLogInfo brokerLogger "Successfully authenticated with IG using legacy auth (migration in progress)"

          -- Create connection with enhanced modular structure
          statusVar <- liftIO $ newTVarIO $ Connected now
          heartbeatVar <- liftIO $ newTVarIO now
          subsVar <- liftIO $ newTVarIO Map.empty
          reconnectVar <- liftIO $ newTVarIO 0
          sessionVar <- liftIO $ newTVarIO $ Just session
          let liveCfg = acLiveTrading appConfig

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
                , bcMaxTicksPerSecond = max 0.1 (ltcMaxTicksPerSecond liveCfg)
                , bcStreamingMode = if isJust (igLightstreamerEndpoint session)
                                    then WebSocketStreaming
                                    else RESTPolling
                }

          -- Store connection
          context <- ask
          let connectionsVar = bcConnections context
          liftIO $ atomically $ do
            connections <- readTVar connectionsVar
            writeTVar connectionsVar (Map.insert connId connection connections)

          return $ Right connId
    _ -> do
      liftIO $ compLogWarn brokerLogger "Missing IG credentials, falling back to demo mode"
      connectDemoModular connId now appConfig

-- Demo connection (fallback)
connectDemoModular :: MonadIO m => ConnectionId -> UTCTime -> AppConfig -> BrokerDataProviderM m (Result ConnectionId)
connectDemoModular connId now appConfig = do
  liftIO $ compLogInfo brokerLogger "Creating demo connection"

  statusVar <- liftIO $ newTVarIO $ Connected now
  heartbeatVar <- liftIO $ newTVarIO now
  subsVar <- liftIO $ newTVarIO Map.empty
  reconnectVar <- liftIO $ newTVarIO 0
  sessionVar <- liftIO $ newTVarIO Nothing
  let liveCfg = acLiveTrading appConfig

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
        , bcMaxTicksPerSecond = max 0.1 (ltcMaxTicksPerSecond liveCfg)
        , bcStreamingMode = RESTPolling  -- Demo mode uses REST polling
        }

  context <- ask
  let connectionsVar = bcConnections context
  liftIO $ atomically $ do
    connections <- readTVar connectionsVar
    writeTVar connectionsVar (Map.insert connId connection connections)

  return $ Right connId

-- IG market data subscription with enhanced error handling
startIGMarketDataSubscription :: MonadIO m => BrokerConnection -> Instrument -> BrokerDataProviderM m ()
startIGMarketDataSubscription conn instrument = do
  let streamingMode = bcStreamingMode conn
  liftIO $ compLogInfo brokerLogger ("Starting IG market data subscription for " <> T.pack (show instrument) <> " using " <> T.pack (show streamingMode))

  maybeSession <- liftIO $ readTVarIO (bcIGSession conn)
  case maybeSession of
    Nothing -> do
      liftIO $ compLogInfo brokerLogger "No IG session available for market data (running in demo/polling mode)"
      startRESTPolling conn instrument

    Just session ->
      case (bcStreamingMode conn, igLightstreamerEndpoint session) of
        (WebSocketStreaming, Just _endpoint) -> do
          liftIO $ compLogInfo brokerLogger "Starting Lightstreamer WebSocket streaming"
          startWebSocketStreaming conn instrument session

        _ -> do
          liftIO $ compLogInfo brokerLogger "No Lightstreamer endpoint or forced REST mode; using REST polling"
          startRESTPolling conn instrument

-- Start WebSocket streaming for an instrument
startWebSocketStreaming :: MonadIO m => BrokerConnection -> Instrument -> IGSession -> BrokerDataProviderM m ()
startWebSocketStreaming conn instrument session = do
  liftIO $ compLogInfo brokerLogger ("Establishing WebSocket streaming for " <> T.pack (show instrument))

  -- Get the tick buffer for this instrument
  subs <- liftIO $ readTVarIO (bcSubscriptions conn)
  case Map.lookup instrument subs of
    Nothing -> do
      liftIO $ compLogError brokerLogger ("No subscription state found for instrument: " <> T.pack (show instrument))

    Just subscriptionState -> do
      -- Start WebSocket streaming in background
      _ <- liftIO $ async $ do
        streamingResult <- IGStreaming.startLightstreamerConnection (bcConfig conn) session
        case streamingResult of
          Left err -> do
            compLogError brokerLogger ("Failed to start Lightstreamer connection: " <> T.pack (show err))
            compLogWarn brokerLogger "Falling back to REST polling"
            IGPolling.igStreamingLoop conn instrument

          Right lsConnection -> do
            compLogInfo brokerLogger ("Lightstreamer connection established for " <> T.pack (show instrument))

            -- Subscribe to price updates
            subResult <- IGStreaming.subscribeToPriceUpdates lsConnection instrument (ssTickBuffer subscriptionState)
            case subResult of
              Left err -> do
                compLogError brokerLogger ("Failed to subscribe to price updates: " <> T.pack (show err))
                compLogWarn brokerLogger "Falling back to REST polling"
                IGStreaming.closeLightstreamerConnection lsConnection
                IGPolling.igStreamingLoop conn instrument

              Right _subscription -> do
                compLogInfo brokerLogger ("Successfully subscribed to WebSocket streaming for " <> T.pack (show instrument))

      liftIO $ compLogInfo brokerLogger ("Started WebSocket streaming setup for " <> T.pack (show instrument))

-- Start REST polling for an instrument (fallback method)
startRESTPolling :: MonadIO m => BrokerConnection -> Instrument -> BrokerDataProviderM m ()
startRESTPolling conn instrument = do
  liftIO $ compLogInfo brokerLogger ("Starting REST API polling for " <> T.pack (show instrument))
  -- Start background polling loop with delay
  _ <- liftIO $ async $ do
    threadDelay 200_000  -- Initial delay so flush is empty
    IGPolling.igStreamingLoop conn instrument
  liftIO $ compLogInfo brokerLogger ("Started REST polling for " <> T.pack (show instrument))

-- Helper to run broker data provider
runBrokerDataProviderIO :: BrokerDataProviderM IO a -> IO a
runBrokerDataProviderIO action = do
  -- Load configuration
  configResult <- loadAppConfig
  case configResult of
    Left err -> error $ "Failed to load config: " <> T.unpack err
    Right appConfig -> do
      connectionsVar <- newTVarIO Map.empty
      let context = BrokerContext
            { bcAppConfig = appConfig
            , bcConnections = connectionsVar
            }
      runReaderT (runBrokerDataProvider action) context

-- Helper functions for IG API integration
sideToIGDirection :: Domain.Types.Side -> Direction
sideToIGDirection Domain.Types.Buy = BUY
sideToIGDirection Domain.Types.Sell = SELL

instrumentToEpic :: Instrument -> Text
instrumentToEpic instrument =
  case IGStreaming.instrumentToIGEpic instrument of
    Just epic -> epic
    Nothing -> error $ "Unsupported instrument: " <> show instrument

-- Trading execution functions with enhanced error handling
executeEnterSignal :: (MonadIO m) => ConnectionId -> Instrument -> Domain.Types.Side -> Double -> BrokerDataProviderM m (Result Text)
executeEnterSignal connId instrument side positionSize = do
  liftIO $ compLogInfo brokerLogger ("Executing ENTER signal (modular): " <> T.pack (show side) <> " " <> T.pack (show positionSize) <> " of " <> T.pack (show instrument))

  context <- ask
  let connectionsVar = bcConnections context
  maybeConn <- liftIO $ atomically $ do
    connections <- readTVar connectionsVar
    return $ Map.lookup connId connections

  case maybeConn of
    Nothing -> do
      liftIO $ compLogError brokerLogger ("Connection not found: " <> T.pack (show connId))
      return $ Left $ brokerError ("Connection not found: " <> T.pack (show connId))

    Just conn -> do
      maybeSession <- liftIO $ readTVarIO (bcIGSession conn)
      case maybeSession of
        Nothing -> do
          liftIO $ compLogWarn brokerLogger "No IG session available - running in demo mode, logging trade only"
          return $ Right "DEMO_TRADE_LOGGED"

        Just session -> do
          -- Generate nanosecond-precision timestamp
          tsNs <- liftIO $ do
            posix <- getPOSIXTime
            let nsTotal = floor (realToFrac posix * 1_000_000_000 :: Double)
            return $ T.pack (show nsTotal)

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
                , dealCurrencyCode = Just "USD"
                , dealForceOpen = Just True
                , dealGuaranteedStop = Just False
                , dealStopLevel = Nothing
                , dealStopDistance = Nothing
                , dealTrailingStop = Nothing
                , dealTrailingStopIncrement = Nothing
                , dealLimitLevel = Nothing
                , dealLimitDistance = Nothing
                , dealTimeInForce = Nothing
                , dealReference = Just (mkDealReference "TEMPEH" (T.pack (show connId) <> T.take 8 epic) tsNs)
                }

          -- Execute the trade using existing deals module
          result <- liftIO $ IGDeals.createPosition (bcConfig conn) session dealReq
          case result of
            Left err -> do
              liftIO $ compLogError brokerLogger ("Failed to create position: " <> T.pack (show err))
              return $ Left err
            Right dealResponse -> do
              liftIO $ compLogInfo brokerLogger ("Position creation submitted: " <> dealResponseReference dealResponse)

              -- Wait and check deal confirmation
              liftIO $ threadDelay 1000000 -- 1 second
              confirmResult <- liftIO $ IGDeals.getDealConfirmation (bcConfig conn) session (dealResponseReference dealResponse)
              case confirmResult of
                Left confErr -> do
                  liftIO $ compLogWarn brokerLogger ("Could not confirm deal: " <> T.pack (show confErr))
                  return $ Right $ dealResponseReference dealResponse
                Right confirmation -> do
                  case confirmationDealStatus confirmation of
                    Just REJECTED -> do
                      let reason = maybe "No reason provided" id (confirmationReason confirmation)
                      liftIO $ compLogError brokerLogger ("Deal REJECTED - Reason: " <> reason)
                      return $ Left $ brokerError ("Deal rejected: " <> reason)
                    Just dealStatus -> do
                      liftIO $ compLogInfo brokerLogger ("Deal confirmed with status: " <> T.pack (show dealStatus))
                      return $ Right $ dealResponseReference dealResponse
                    Nothing -> do
                      liftIO $ compLogWarn brokerLogger "Deal confirmation received but no status provided"
                      return $ Right $ dealResponseReference dealResponse

executeExitSignal :: (MonadIO m) => ConnectionId -> Instrument -> BrokerDataProviderM m (Result Text)
executeExitSignal connId instrument = do
  liftIO $ compLogInfo brokerLogger ("Executing EXIT signal (modular) for " <> T.pack (show instrument))

  context <- ask
  let connectionsVar = bcConnections context
  maybeConn <- liftIO $ atomically $ do
    connections <- readTVar connectionsVar
    return $ Map.lookup connId connections

  case maybeConn of
    Nothing -> do
      liftIO $ compLogError brokerLogger ("Connection not found: " <> T.pack (show connId))
      return $ Left $ brokerError ("Connection not found: " <> T.pack (show connId))

    Just conn -> do
      maybeSession <- liftIO $ readTVarIO (bcIGSession conn)
      case maybeSession of
        Nothing -> do
          liftIO $ compLogWarn brokerLogger "No IG session available - running in demo mode, logging exit only"
          return $ Right "DEMO_EXIT_LOGGED"

        Just session -> do
          -- Get current positions for this instrument (enhanced error handling)
          positionsResult <- liftIO $ IGDeals.getPositions (bcConfig conn) session
          case positionsResult of
            Left err -> do
              liftIO $ compLogError brokerLogger ("Failed to get positions: " <> T.pack (show err))
              return $ Left err
            Right positions -> do
              liftIO $ compLogInfo brokerLogger ("Found " <> T.pack (show (length positions)) <> " open positions")
              return $ Right "DEMO_EXIT_LOGGED"  -- For now, just log


-- Deal reference generation (for tests)
mkDealReference :: Text -> Text -> Text -> Text
mkDealReference prefix connInfo timestamp =
  let -- Limit each component and remove non-alphanumeric characters
      cleanPrefix = T.take 6 $ T.filter isAlphaNum prefix
      cleanConnInfo = T.take 8 $ T.filter isAlphaNum connInfo
      cleanTimestamp = T.take 12 $ T.filter isAlphaNum timestamp
      -- Concatenate without separators to ensure alphanumeric
      combined = cleanPrefix <> cleanConnInfo <> cleanTimestamp
      -- Ensure final result is <= 30 characters
      final = T.take 30 combined
      -- If the result is empty after filtering, provide a default alphanumeric fallback
  in if T.null final then "TEMPEH001" else final
