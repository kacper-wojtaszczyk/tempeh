{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Adapter.IG.Streaming
  ( -- Core streaming functions
    startLightstreamerConnection
  , subscribeToPriceUpdates
  , closeLightstreamerConnection
  , LSConnection(..)
  , LSSubscription(..)
  , LSMessage(..)
  , LSControlMessage(..)

  -- Protocol handling
  , parseLightstreamerMessage
  , formatControlMessage
  , handleStreamingTick

  -- Connection management
  , createLightstreamerSession
  , bindLightstreamerSession

  -- Utility functions
  , instrumentToIGEpic
  ) where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (try, SomeException)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime(..), getCurrentTime)
import qualified Network.WebSockets as WS
import qualified Wuss

import Adapter.IG.Types
import Domain.Types
import Util.Config (BrokerConfig(..))
import Util.Error (Result, brokerError)
import Util.Logger (logInfo, logError, logWarn, logDebug, ComponentName(..), runFileLoggerWithComponent)

-- Enhanced Lightstreamer connection and message types
data LSConnection = LSConnection
  { lsConnection :: WS.Connection
  , lsSessionId :: Maybe Text
  , lsControlUrl :: Text
  , lsSubscriptions :: TVar (Map Int LSSubscription)
  , lsNextSubId :: TVar Int
  , lsHeartbeatAsync :: Maybe (Async ())
  , lsMessageAsync :: Maybe (Async ())
  , lsTableMap :: TVar (Map Int Int)        -- serverTableId -> subId
  , lsPendingSubs :: TVar [Int]             -- queue of subIds awaiting SUBOK
  , lsRequestId :: TVar Int                 -- for generating unique request IDs
  }

data LSSubscription = LSSubscription
  { lsSubId :: Int
  , lsItems :: [Text]          -- e.g., ["CHART:CS.D.EURUSD.MINI.IP:TICK"]
  , lsFields :: [Text]         -- e.g., ["BID", "OFR", "LTP", "LTV", "TTV", "UTM"]
  , lsInstrument :: Instrument
  , lsTickBuffer :: TVar [Tick]
  , lsSnapshotComplete :: TVar Bool        -- tracks EOS notification
  , lsCurrentFreq :: TVar (Maybe Text)     -- tracks CONF notifications
  }

-- Comprehensive TLCP message types matching specification
data LSMessage
  = -- Session management
    LSConOk Text [(Text, Text)]                    -- sessionId, params
  | LSConErr Int Text                              -- errorCode, errorMessage
  | LSEnd Int Text                                 -- causeCode, causeMessage
  | LSWsOk

  -- Request responses
  | LSReqOk Int                                    -- reqId
  | LSReqErr Int Int Text                          -- reqId, errorCode, errorMessage
  | LSError Int Text                               -- errorCode, errorMessage

  -- Subscription management
  | LSSubOk Int Int Int                            -- subId, numItems, numFields
  | LSSubCmd Int Int Int                           -- subId, numItems, numFields
  | LSUnSub Int                                    -- subId
  | LSEos Int                                      -- subId (end of snapshot)
  | LSCs Int                                       -- subId (snapshot cleared)
  | LSOv Int                                       -- subId (buffer overflow)
  | LSConf Int Text                                -- subId, newFrequency

  -- Data updates
  | LSUpdate Int Int [Maybe Text]                  -- subId, itemId, fieldValues

  -- Connection management
  | LSPing
  | LSProbe
  | LSLoop
  | LSNoop
  | LSSync
  | LSProg Int                                     -- count

  -- Information
  | LSClientIp Text                                -- ipAddress
  | LSServName Text                                -- serverName
  | LSCons Text                                    -- bandwidth

  -- Message handling
  | LSMsgDone Text Int                             -- sequence, prog
  | LSMsgFail Text Int                             -- sequence, prog

  -- Unknown/info messages
  | LSInfo Text
  deriving (Show, Eq)

data LSControlMessage
  = CreateSession Text Text            -- control_url, adapter_set
  | BindSession Text Text             -- session_id, connection_id
  | Subscribe [Text] [Text] Text      -- items, fields, mode
  | Unsubscribe Int                   -- subscription_id
  deriving (Show)

-- Streaming logging helpers
streamLogInfo :: Text -> IO ()
streamLogInfo msg = runFileLoggerWithComponent (ComponentName "STREAM") $ logInfo msg

streamLogWarn :: Text -> IO ()
streamLogWarn msg = runFileLoggerWithComponent (ComponentName "STREAM") $ logWarn msg

streamLogError :: Text -> IO ()
streamLogError msg = runFileLoggerWithComponent (ComponentName "STREAM") $ logError msg

streamLogDebug :: Text -> IO ()
streamLogDebug msg = runFileLoggerWithComponent (ComponentName "STREAM") $ logDebug msg

-- Main streaming connection function
startLightstreamerConnection :: BrokerConfig -> IGSession -> IO (Result LSConnection)
startLightstreamerConnection config session = do
  streamLogInfo "Establishing Lightstreamer WebSocket connection with TLCP authentication"

  case igLightstreamerEndpoint session of
    Nothing -> do
      streamLogError "No Lightstreamer endpoint in IG session"
      return $ Left $ brokerError "No Lightstreamer endpoint available"

    Just lsEndpoint -> do
      streamLogInfo $ "Connecting to Lightstreamer: " <> lsEndpoint

      -- Establish WebSocket connection with proper error handling
      result <- try $ establishLightstreamerConnection lsEndpoint session
      case result of
        Left (ex :: SomeException) -> do
          streamLogError $ "Lightstreamer connection failed: " <> T.pack (show ex)
          return $ Left $ brokerError $ "Lightstreamer connection failed: " <> T.pack (show ex)

        Right lsConn -> do
          streamLogInfo "Lightstreamer connection ready"
          return $ Right lsConn

-- Establish complete Lightstreamer connection with proper TLCP protocol
establishLightstreamerConnection :: Text -> IGSession -> IO LSConnection
establishLightstreamerConnection lsEndpoint igSession = do
  -- Parse the endpoint URL to extract host
  let host = T.unpack $ T.replace "https://" "" $ T.replace "http://" "" lsEndpoint
      path = "/lightstreamer"

  streamLogDebug $ "Connecting to host: " <> T.pack host <> ", path: " <> T.pack path
  streamLogDebug $ "Using TLCP-2.4.0.lightstreamer.com subprotocol"

  streamLogInfo "Starting WebSocket connection with TLCP subprotocol..."

  -- MVar to pass connection back once ready
  readyVar <- newEmptyMVar

  -- Start WebSocket client in a managed async thread and keep it alive
  _ <- async $ do
    let connectionOptions = WS.defaultConnectionOptions { WS.connectionCompressionOptions = WS.NoCompression }
        headers = [("Sec-WebSocket-Protocol", BS8.pack "TLCP-2.4.0.lightstreamer.com")]
    Wuss.runSecureClientWith host 443 path connectionOptions headers $ \conn -> do
      streamLogInfo "WebSocket handshake completed successfully with TLCP!"

      -- Initialize connection state
      subscriptions <- newTVarIO Map.empty
      nextSubId <- newTVarIO 1
      tableMap <- newTVarIO Map.empty
      pendingSubs <- newTVarIO []
      requestId <- newTVarIO 1

      -- Create Lightstreamer session using TLCP protocol
      streamLogDebug "Starting TLCP session creation"
      sessionResult <- createLightstreamerSessionTLCP conn igSession lsEndpoint
      case sessionResult of
        Left err -> do
          streamLogError $ "Failed to create Lightstreamer session: " <> T.pack (show err)
          putMVar readyVar (error "Lightstreamer session creation failed")
        Right sessionId -> do
          -- Build LSConnection and signal readiness
          let lsConn = LSConnection
                { lsConnection = conn
                , lsSessionId = Just sessionId
                , lsControlUrl = lsEndpoint
                , lsSubscriptions = subscriptions
                , lsNextSubId = nextSubId
                , lsHeartbeatAsync = Nothing
                , lsMessageAsync = Nothing
                , lsTableMap = tableMap
                , lsPendingSubs = pendingSubs
                , lsRequestId = requestId
                }
          putMVar readyVar lsConn
          -- Run message loop to keep connection alive
          handleLightstreamerMessages lsConn

  -- Wait for the connection to be ready
  takeMVar readyVar

-- Percent-encode reserved characters in TLCP parameter values
-- According to TLCP spec, only these characters are reserved: CR, LF, &, =, %, +
-- Note: Pipe (|) is NOT a reserved character in TLCP and should not be encoded
percentEncodeTLCP :: Text -> Text
percentEncodeTLCP text =
  T.replace "\r" "%0D" $
  T.replace "\n" "%0A" $
  T.replace "&" "%26" $
  T.replace "=" "%3D" $
  T.replace "%" "%25" $
  T.replace "+" "%2B" $
  T.replace " " "%20" text
  -- Note: Removed pipe (|) and colon (:) encoding as they are not TLCP reserved characters

-- Create Lightstreamer session using proper TLCP protocol
createLightstreamerSessionTLCP :: WS.Connection -> IGSession -> Text -> IO (Result Text)
createLightstreamerSessionTLCP conn igSession _lsEndpoint = do
  streamLogInfo "Creating TLCP session with IG authentication"

  -- Create authentication parameters
  let rawPassword = "CST-" <> igCST igSession <> "|XST-" <> igXSecurityToken igSession
      authPassword = percentEncodeTLCP rawPassword
      authUser = igSessionToken igSession

  -- Construct TLCP session parameters
  let params = T.intercalate "&"
        [ "LS_cid=mgQkwtwdysogQz2BJ4Ji%20kOj2Bg"
        , "LS_adapter_set=DEFAULT"
        , "LS_user=" <> authUser
        , "LS_password=" <> authPassword
        , "LS_keepalive_millis=30000"
        , "LS_send_sync=false"
        , "LS_reduce_head=false"
        ]
      sessionMsg = "create_session\r\n" <> params <> "\r\n"

  streamLogInfo "Sending TLCP session creation request"
  WS.sendTextData conn sessionMsg

  -- Wait for response
  resp <- WS.receiveData conn
  let responseText = TE.decodeUtf8 $ LBS.toStrict resp

  case parseLightstreamerMessage (T.strip responseText) of
    LSConOk sessionId params -> do
      streamLogInfo $ "TLCP session created successfully: " <> sessionId
      return $ Right sessionId
    LSConErr errorCode errorMessage -> do
      streamLogError $ "TLCP session creation failed (error " <> T.pack (show errorCode) <> "): " <> errorMessage
      return $ Left $ brokerError $ "Session creation failed: " <> errorMessage
    LSError errorCode errorMessage -> do
      streamLogError $ "TLCP session creation error (code " <> T.pack (show errorCode) <> "): " <> errorMessage
      return $ Left $ brokerError $ "Session creation error: " <> errorMessage
    other -> do
      streamLogError $ "Unexpected TLCP response: " <> T.pack (show other)
      return $ Left $ brokerError $ "Unexpected session creation response"

-- Subscribe to price updates for an instrument using CHART TICK
subscribeToPriceUpdates :: LSConnection -> Instrument -> TVar [Tick] -> IO (Result LSSubscription)
subscribeToPriceUpdates lsConn instrument tickBuffer = do
  streamLogInfo $ "Subscribing to CHART TICK updates for: " <> T.pack (show instrument)

  -- Convert instrument to IG market epic
  case instrumentToIGEpic instrument of
    Nothing -> do
      streamLogError $ "Unsupported instrument for streaming: " <> T.pack (show instrument)
      return $ Left $ brokerError $ "Unsupported instrument for streaming: " <> T.pack (show instrument)

    Just epic -> do
      -- Get next subscription ID and request ID
      (subId, reqId) <- atomically $ do
        currentSub <- readTVar (lsNextSubId lsConn)
        writeTVar (lsNextSubId lsConn) (currentSub + 1)
        currentReq <- readTVar (lsRequestId lsConn)
        writeTVar (lsRequestId lsConn) (currentReq + 1)
        return (currentSub, currentReq)

      -- Use CHART TICK subscription for tick data with proper format
      let chartId = "CHART:" <> epic <> ":TICK"
          -- Fields as specified in IG documentation for CHART TICK
          fields = ["BID", "OFR", "LTP", "LTV", "TTV", "UTM", "DAY_OPEN_MID", "DAY_NET_CHG_MID", "DAY_HIGH", "DAY_LOW"]

      streamLogDebug $ "Subscribing to chart: " <> chartId <> " with fields: " <> T.intercalate ", " fields

      -- Create subscription record with enhanced state tracking
      snapshotComplete <- newTVarIO False
      currentFreq <- newTVarIO Nothing
      let subscription = LSSubscription
            { lsSubId = subId
            , lsItems = [chartId]
            , lsFields = fields
            , lsInstrument = instrument
            , lsTickBuffer = tickBuffer
            , lsSnapshotComplete = snapshotComplete
            , lsCurrentFreq = currentFreq
            }

      -- Add to subscriptions map
      atomically $ modifyTVar (lsSubscriptions lsConn) $ Map.insert subId subscription
      -- Mark as pending until SUBOK arrives
      atomically $ modifyTVar (lsPendingSubs lsConn) (++ [subId])

      -- Send subscription message using proper TLCP protocol format
      let sessionId = fromMaybe "" (lsSessionId lsConn)
          fieldsText = T.intercalate " " fields
          groupText = chartId
          params = T.intercalate "&"
            [ "LS_reqId=" <> T.pack (show reqId)
            , "LS_op=add"
            , "LS_subId=" <> T.pack (show subId)
            , "LS_mode=DISTINCT"
            , "LS_group=" <> groupText
            , "LS_schema=" <> fieldsText
            , "LS_snapshot=true"
            , "LS_ack=true"
            , "LS_session=" <> sessionId
            ]
          subMsg = "control\r\n" <> params <> "\r\n"

      streamLogInfo $ "Sending TLCP subscription request (reqId=" <> T.pack (show reqId) <> ", subId=" <> T.pack (show subId) <> ")"
      WS.sendTextData (lsConnection lsConn) subMsg

      streamLogInfo $ "CHART TICK subscription created with ID: " <> T.pack (show subId)
      return $ Right subscription

-- Convert instrument to IG epic format
instrumentToIGEpic :: Instrument -> Maybe Text
instrumentToIGEpic (Instrument "EURUSD") = Just "CS.D.EURUSD.MINI.IP"
instrumentToIGEpic (Instrument "GBPUSD") = Just "CS.D.GBPUSD.MINI.IP"
instrumentToIGEpic (Instrument "USDJPY") = Just "CS.D.USDJPY.MINI.IP"
instrumentToIGEpic (Instrument "AUDUSD") = Just "CS.D.AUDUSD.MINI.IP"
instrumentToIGEpic (Instrument "USDCAD") = Just "CS.D.USDCAD.MINI.IP"
instrumentToIGEpic _ = Nothing  -- No fallback, return Nothing for unsupported instruments

-- Handle incoming Lightstreamer messages with comprehensive TLCP support
handleLightstreamerMessages :: LSConnection -> IO ()
handleLightstreamerMessages lsConn = do
  streamLogInfo "Starting Lightstreamer message handler"

  let handleFrame = do
        frame <- WS.receiveData (lsConnection lsConn)
        let txt = TE.decodeUtf8 $ LBS.toStrict frame
            lines' = filter (not . T.null) $ map T.strip $ T.splitOn "\r\n" txt
        mapM_ handleLine lines'

      handleLine responseText = do
        streamLogDebug $ "Received: " <> responseText
        case parseLightstreamerMessage responseText of
          -- Session management responses
          LSConOk sessionId params -> do
            streamLogInfo $ "Session confirmed: " <> sessionId
            streamLogDebug $ "Session params: " <> T.pack (show params)

          LSConErr errorCode errorMessage -> do
            streamLogError $ "Connection error " <> T.pack (show errorCode) <> ": " <> errorMessage

          LSEnd causeCode causeMessage -> do
            streamLogWarn $ "Session ended (cause " <> T.pack (show causeCode) <> "): " <> causeMessage

          LSWsOk -> do
            streamLogDebug "WebSocket connection confirmed"

          -- Request responses
          LSReqOk reqId -> do
            streamLogDebug $ "Request " <> T.pack (show reqId) <> " acknowledged"

          LSReqErr reqId errorCode errorMessage -> do
            streamLogError $ "Request " <> T.pack (show reqId) <> " failed (error " <> T.pack (show errorCode) <> "): " <> errorMessage
            -- Handle subscription errors
            atomically $ do
              pend <- readTVar (lsPendingSubs lsConn)
              case find (== reqId) pend of
                Just failedSubId -> do
                  writeTVar (lsPendingSubs lsConn) (filter (/= failedSubId) pend)
                  modifyTVar (lsSubscriptions lsConn) (Map.delete failedSubId)
                Nothing -> return ()

          LSError errorCode errorMessage -> do
            streamLogError $ "Protocol error " <> T.pack (show errorCode) <> ": " <> errorMessage

          -- Subscription management
          LSSubOk subId numItems numFields -> do
            streamLogInfo $ "Subscription " <> T.pack (show subId) <> " confirmed (" <> T.pack (show numItems) <> " items, " <> T.pack (show numFields) <> " fields)"
            -- Remove from pending list
            atomically $ do
              pend <- readTVar (lsPendingSubs lsConn)
              writeTVar (lsPendingSubs lsConn) (filter (/= subId) pend)

          LSSubCmd subId numItems numFields -> do
            streamLogInfo $ "Command subscription " <> T.pack (show subId) <> " confirmed (" <> T.pack (show numItems) <> " items, " <> T.pack (show numFields) <> " fields)"

          LSUnSub subId -> do
            streamLogInfo $ "Subscription " <> T.pack (show subId) <> " unsubscribed"
            atomically $ modifyTVar (lsSubscriptions lsConn) (Map.delete subId)

          LSEos subId -> do
            streamLogDebug $ "End of snapshot for subscription " <> T.pack (show subId)
            subs <- readTVarIO (lsSubscriptions lsConn)
            case Map.lookup subId subs of
              Just sub -> atomically $ writeTVar (lsSnapshotComplete sub) True
              Nothing -> streamLogWarn $ "EOS for unknown subscription: " <> T.pack (show subId)

          LSCs subId -> do
            streamLogDebug $ "Snapshot cleared for subscription " <> T.pack (show subId)
            subs <- readTVarIO (lsSubscriptions lsConn)
            case Map.lookup subId subs of
              Just sub -> atomically $ writeTVar (lsSnapshotComplete sub) False
              Nothing -> streamLogWarn $ "CS for unknown subscription: " <> T.pack (show subId)

          LSOv subId -> do
            streamLogWarn $ "Buffer overflow for subscription " <> T.pack (show subId)

          LSConf subId newFrequency -> do
            streamLogDebug $ "Frequency changed for subscription " <> T.pack (show subId) <> " to " <> newFrequency
            subs <- readTVarIO (lsSubscriptions lsConn)
            case Map.lookup subId subs of
              Just sub -> atomically $ writeTVar (lsCurrentFreq sub) (Just newFrequency)
              Nothing -> streamLogWarn $ "CONF for unknown subscription: " <> T.pack (show subId)

          -- Data updates
          LSUpdate subId itemId values -> do
            streamLogDebug $ "Processing update for subscription " <> T.pack (show subId) <> ", item " <> T.pack (show itemId)
            handleStreamingTick lsConn subId values

          -- Connection management
          LSPing -> do
            streamLogDebug "Received ping, sending pong"
            WS.sendTextData (lsConnection lsConn) ("PONG" :: Text)

          LSProbe -> do
            streamLogDebug "Received probe, sending pong"
            WS.sendTextData (lsConnection lsConn) ("PONG" :: Text)

          LSLoop -> do
            streamLogDebug "Received loop message, rebinding session"
            case lsSessionId lsConn of
              Just sid -> do
                let bindMsg = "bind_session\r\nLS_session=" <> sid <> "&LS_keepalive_millis=30000&LS_send_sync=false&LS_cause=ws.loop\r\n"
                streamLogDebug $ "Sending bind_session: " <> bindMsg
                WS.sendTextData (lsConnection lsConn) bindMsg
              Nothing ->
                streamLogWarn "Loop received but no session id present"

          LSNoop -> do
            streamLogDebug "Received NOOP keepalive"

          LSSync -> do
            streamLogDebug "Received SYNC notification"

          LSProg count -> do
            streamLogDebug $ "Received PROG: " <> T.pack (show count)

          -- Information messages
          LSClientIp ipAddress -> do
            streamLogDebug $ "Client IP: " <> ipAddress

          LSServName serverName -> do
            streamLogDebug $ "Server name: " <> serverName

          LSCons bandwidth -> do
            streamLogDebug $ "Bandwidth constraint: " <> bandwidth

          -- Message handling
          LSMsgDone sequence prog -> do
            streamLogDebug $ "Message done: sequence=" <> sequence <> ", prog=" <> T.pack (show prog)

          LSMsgFail sequence prog -> do
            streamLogWarn $ "Message failed: sequence=" <> sequence <> ", prog=" <> T.pack (show prog)

          -- Other messages
          LSInfo infoMsg ->
            streamLogDebug $ "Received server info: " <> infoMsg

  result <- try $ forever handleFrame
  case result of
    Left (ex :: SomeException) -> do
      streamLogError $ "Message handler error: " <> T.pack (show ex)
    Right _ -> do
      streamLogInfo "Message handler completed"

-- Handle streaming tick data from CHART TICK subscription
handleStreamingTick :: LSConnection -> Int -> [Maybe Text] -> IO ()
handleStreamingTick lsConn subId values = do
  subs <- readTVarIO (lsSubscriptions lsConn)
  case Map.lookup subId subs of
    Nothing ->
      streamLogWarn $ "Received tick for unknown subscription: " <> T.pack (show subId)

    Just subscription -> do
      -- Extract tick data from CHART TICK subscription values
      -- Fields: ["BID", "OFR", "LTP", "LTV", "TTV", "UTM", "DAY_OPEN_MID", "DAY_NET_CHG_MID", "DAY_HIGH", "DAY_LOW"]
      case values of
        (bidMaybe:offerMaybe:_ltpMaybe:_ltvMaybe:_ttvMaybe:timeMaybe:_rest) -> do
          currentTime <- getCurrentTime

          let bid = bidMaybe >>= parseScientific >>= Just . realToFrac
              offer = offerMaybe >>= parseScientific >>= Just . realToFrac
              tickTime = fromMaybe currentTime $ timeMaybe >>= parseIGTimestamp

          case (bid, offer) of
            (Just bidPrice, Just offerPrice) -> do
              let tick = Tick
                    { tTime = tickTime
                    , tInstr = lsInstrument subscription
                    , tBid = Price bidPrice
                    , tAsk = Price offerPrice
                    , tVolume = Nothing
                    }

              -- Add tick to buffer
              atomically $ modifyTVar (lsTickBuffer subscription) (++ [tick])

            _ ->
              streamLogWarn $ "Invalid price data in CHART TICK: bid=" <> T.pack (show bid) <> ", offer=" <> T.pack (show offer)

        _ ->
          streamLogWarn $ "Unexpected field count for CHART TICK data: expected at least 6 fields, got " <> T.pack (show $ length values) <> " values"

-- Parse scientific number from text
parseScientific :: Text -> Maybe Scientific
parseScientific t = case reads (T.unpack t) of
  [(val, "")] -> Just val
  _ -> Nothing

-- Parse IG timestamp format (Unix timestamp with milliseconds)
parseIGTimestamp :: Text -> Maybe UTCTime
parseIGTimestamp timeStr =
  case reads (T.unpack timeStr) of
    [(timestamp :: Integer, "")] -> do
      let seconds = fromIntegral (timestamp `div` 1000)
          picoseconds = fromIntegral ((timestamp `mod` 1000) * 1_000_000_000)
      return $ UTCTime (toEnum (fromIntegral seconds `div` 86400 + 40587)) (fromIntegral (fromIntegral seconds `mod` 86400) + picoseconds / 1_000_000_000_000)
    _ -> Nothing

-- Close Lightstreamer connection
closeLightstreamerConnection :: LSConnection -> IO ()
closeLightstreamerConnection lsConn = do
  streamLogInfo "Closing Lightstreamer connection"

  -- Cancel async tasks
  case lsMessageAsync lsConn of
    Just async -> cancel async
    Nothing -> return ()

  case lsHeartbeatAsync lsConn of
    Just async -> cancel async
    Nothing -> return ()

  streamLogInfo "Lightstreamer connection closed"

-- Legacy functions for compatibility (not used in TLCP implementation)
createLightstreamerSession :: LSConnection -> IGSession -> IO (Result Text)
createLightstreamerSession lsConn igSession = do
  streamLogWarn "Legacy createLightstreamerSession called - using TLCP implementation"
  createLightstreamerSessionTLCP (lsConnection lsConn) igSession (lsControlUrl lsConn)

bindLightstreamerSession :: LSConnection -> Text -> Text -> IO (Result ())
bindLightstreamerSession lsConn sessionId connectionId = do
  streamLogWarn "Legacy bindLightstreamerSession called - TLCP handles binding automatically"
  return $ Right ()

formatControlMessage :: LSControlMessage -> Text
formatControlMessage msg = case msg of
  CreateSession controlUrl adapterSet ->
    "create_session\r\nLS_adapter_set=" <> adapterSet <>
    "&LS_cid=mgQkwtwdysogQz2BJ4Ji%20kOj2Bg&" <>
    "LS_send_sync=false&LS_cause=api\r\n"

  BindSession sessionId connectionId ->
    "bind_session\r\nLS_session=" <> sessionId <>
    "&LS_keepalive_millis=30000&LS_send_sync=false&" <>
    "LS_cause=ws.loop\r\n"

  Subscribe items fields mode ->
    let itemsStr = T.intercalate " " items
        fieldsStr = T.intercalate " " fields
    in "control\r\nLS_reqId=1&LS_op=add&LS_mode=" <> mode <>
       "&LS_group=" <> itemsStr <> "&LS_schema=" <> fieldsStr <> "\r\n"

  Unsubscribe subId ->
    "control\r\nLS_reqId=" <> T.pack (show subId) <> "&LS_op=delete\r\n"

-- Comprehensive TLCP message parser matching specification
parseLightstreamerMessage :: Text -> LSMessage
parseLightstreamerMessage msg
  -- Session management
  | "CONOK" `T.isPrefixOf` msg = parseConOk msg
  | "CONERR" `T.isPrefixOf` msg = parseConErr msg
  | "END" `T.isPrefixOf` msg = parseEnd msg
  | "WSOK" == T.strip msg = LSWsOk

  -- Request responses
  | "REQOK" `T.isPrefixOf` msg = parseReqOk msg
  | "REQERR" `T.isPrefixOf` msg = parseReqErr msg
  | "ERROR" `T.isPrefixOf` msg = parseError msg

  -- Subscription management
  | "SUBOK" `T.isPrefixOf` msg = parseSubOk msg
  | "SUBCMD" `T.isPrefixOf` msg = parseSubCmd msg
  | "UNSUB" `T.isPrefixOf` msg = parseUnSub msg
  | "EOS" `T.isPrefixOf` msg = parseEos msg
  | "CS" `T.isPrefixOf` msg = parseCs msg
  | "OV" `T.isPrefixOf` msg = parseOv msg
  | "CONF" `T.isPrefixOf` msg = parseConf msg

  -- Data updates
  | "U" `T.isPrefixOf` msg = parseUpdate msg

  -- Connection management
  | "PING" == T.strip msg = LSPing
  | "PROBE" == T.strip msg = LSProbe
  | "LOOP" `T.isPrefixOf` msg = LSLoop
  | "NOOP" == T.strip msg = LSNoop
  | "SYNC" == T.strip msg = LSSync
  | "PROG" `T.isPrefixOf` msg = parseProg msg

  -- Information
  | "CLIENTIP" `T.isPrefixOf` msg = parseClientIp msg
  | "SERVNAME" `T.isPrefixOf` msg = parseServName msg
  | "CONS" `T.isPrefixOf` msg = parseCons msg

  -- Message handling
  | "MSGDONE" `T.isPrefixOf` msg = parseMsgDone msg
  | "MSGFAIL" `T.isPrefixOf` msg = parseMsgFail msg

  -- Unknown messages
  | otherwise = LSInfo msg

-- Individual parser functions for each message type
parseConOk :: Text -> LSMessage
parseConOk msg = case T.splitOn "," msg of
  ("CONOK":sessionId:rest) ->
    let params = concatMap parseParams rest
    in LSConOk sessionId params
  _ -> LSInfo msg
  where
    parseParams param = case T.splitOn "=" param of
      [key, value] -> [(key, value)]
      _ -> []

parseConErr :: Text -> LSMessage
parseConErr msg = case T.splitOn "," msg of
  ("CONERR":errorCodeStr:errorMessageParts) ->
    case reads (T.unpack errorCodeStr) of
      [(errorCode, "")] -> LSConErr errorCode (T.intercalate "," errorMessageParts)
      _ -> LSInfo msg
  _ -> LSInfo msg

parseEnd :: Text -> LSMessage
parseEnd msg = case T.splitOn "," msg of
  ("END":causeCodeStr:causeMessageParts) ->
    case reads (T.unpack causeCodeStr) of
      [(causeCode, "")] -> LSEnd causeCode (T.intercalate "," causeMessageParts)
      _ -> LSInfo msg
  _ -> LSInfo msg

parseReqOk :: Text -> LSMessage
parseReqOk msg = case T.splitOn "," msg of
  ("REQOK":reqIdStr:_) ->
    case reads (T.unpack reqIdStr) of
      [(reqId, "")] -> LSReqOk reqId
      _ -> LSInfo msg
  _ -> LSInfo msg

parseReqErr :: Text -> LSMessage
parseReqErr msg = case T.splitOn "," msg of
  ("REQERR":reqIdStr:errorCodeStr:errorMessageParts) ->
    case (reads (T.unpack reqIdStr), reads (T.unpack errorCodeStr)) of
      ([(reqId, "")], [(errorCode, "")]) ->
        LSReqErr reqId errorCode (T.intercalate "," errorMessageParts)
      _ -> LSInfo msg
  _ -> LSInfo msg

parseError :: Text -> LSMessage
parseError msg = case T.splitOn "," msg of
  ("ERROR":errorCodeStr:errorMessageParts) ->
    case reads (T.unpack errorCodeStr) of
      [(errorCode, "")] -> LSError errorCode (T.intercalate "," errorMessageParts)
      _ -> LSInfo msg
  _ -> LSInfo msg

parseSubOk :: Text -> LSMessage
parseSubOk msg = case T.splitOn "," msg of
  ("SUBOK":subIdStr:numItemsStr:numFieldsStr:_) ->
    case (reads (T.unpack subIdStr), reads (T.unpack numItemsStr), reads (T.unpack numFieldsStr)) of
      ([(subId, "")], [(numItems, "")], [(numFields, "")]) ->
        LSSubOk subId numItems numFields
      _ -> LSInfo msg
  _ -> LSInfo msg

parseSubCmd :: Text -> LSMessage
parseSubCmd msg = case T.splitOn "," msg of
  ("SUBCMD":subIdStr:numItemsStr:numFieldsStr:_) ->
    case (reads (T.unpack subIdStr), reads (T.unpack numItemsStr), reads (T.unpack numFieldsStr)) of
      ([(subId, "")], [(numItems, "")], [(numFields, "")]) ->
        LSSubCmd subId numItems numFields
      _ -> LSInfo msg
  _ -> LSInfo msg

parseUnSub :: Text -> LSMessage
parseUnSub msg = case T.splitOn "," msg of
  ("UNSUB":subIdStr:_) ->
    case reads (T.unpack subIdStr) of
      [(subId, "")] -> LSUnSub subId
      _ -> LSInfo msg
  _ -> LSInfo msg

parseEos :: Text -> LSMessage
parseEos msg = case T.splitOn "," msg of
  ("EOS":subIdStr:_) ->
    case reads (T.unpack subIdStr) of
      [(subId, "")] -> LSEos subId
      _ -> LSInfo msg
  _ -> LSInfo msg

parseCs :: Text -> LSMessage
parseCs msg = case T.splitOn "," msg of
  ("CS":subIdStr:_) ->
    case reads (T.unpack subIdStr) of
      [(subId, "")] -> LSCs subId
      _ -> LSInfo msg
  _ -> LSInfo msg

parseOv :: Text -> LSMessage
parseOv msg = case T.splitOn "," msg of
  ("OV":subIdStr:_) ->
    case reads (T.unpack subIdStr) of
      [(subId, "")] -> LSOv subId
      _ -> LSInfo msg
  _ -> LSInfo msg

parseConf :: Text -> LSMessage
parseConf msg = case T.splitOn "," msg of
  ("CONF":subIdStr:newFrequency:_) ->
    case reads (T.unpack subIdStr) of
      [(subId, "")] -> LSConf subId newFrequency
      _ -> LSInfo msg
  _ -> LSInfo msg

parseUpdate :: Text -> LSMessage
parseUpdate msg = case T.splitOn "," msg of
  ("U":subIdStr:itemIdStr:fieldData:_) ->
    case (reads (T.unpack subIdStr), reads (T.unpack itemIdStr)) of
      ([(subId, "")], [(itemId, "")]) ->
        let fieldValues = map (\v -> if T.null v then Nothing else Just v) $ T.splitOn "|" fieldData
        in LSUpdate subId itemId fieldValues
      _ -> LSInfo msg
  _ -> LSInfo msg

parseProg :: Text -> LSMessage
parseProg msg = case T.splitOn "," msg of
  ("PROG":countStr:_) ->
    case reads (T.unpack countStr) of
      [(count, "")] -> LSProg count
      _ -> LSInfo msg
  _ -> LSInfo msg

parseClientIp :: Text -> LSMessage
parseClientIp msg = case T.splitOn "," msg of
  ("CLIENTIP":ipAddress:_) -> LSClientIp ipAddress
  _ -> LSInfo msg

parseServName :: Text -> LSMessage
parseServName msg = case T.splitOn "," msg of
  ("SERVNAME":serverName:_) -> LSServName serverName
  _ -> LSInfo msg

parseCons :: Text -> LSMessage
parseCons msg = case T.splitOn "," msg of
  ("CONS":bandwidth:_) -> LSCons bandwidth
  _ -> LSInfo msg

parseMsgDone :: Text -> LSMessage
parseMsgDone msg = case T.splitOn "," msg of
  ("MSGDONE":sequence:progStr:_) ->
    case reads (T.unpack progStr) of
      [(prog, "")] -> LSMsgDone sequence prog
      _ -> LSInfo msg
  _ -> LSInfo msg

parseMsgFail :: Text -> LSMessage
parseMsgFail msg = case T.splitOn "," msg of
  ("MSGFAIL":sequence:progStr:_) ->
    case reads (T.unpack progStr) of
      [(prog, "")] -> LSMsgFail sequence prog
      _ -> LSInfo msg
  _ -> LSInfo msg
