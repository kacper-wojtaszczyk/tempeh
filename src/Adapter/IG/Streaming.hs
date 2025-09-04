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

import Control.Concurrent.Async (Async, async, cancel, race)
import Control.Concurrent.STM
import Control.Exception (try, SomeException, bracket, finally)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, getCurrentTime, parseTimeM, defaultTimeLocale)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import qualified Wuss

import Adapter.IG.Types
import Domain.Types
import Util.Config (BrokerConfig(..))
import Util.Error (Result, TempehError(..), BrokerErrorDetails(..), brokerError, getRecoveryStrategy, RecoveryStrategy(..), ErrorSeverity(..))
import Util.Logger (logInfo, logError, logWarn, logDebug, ComponentName(..), runFileLoggerWithComponent)

-- Lightstreamer connection and message types
data LSConnection = LSConnection
  { lsConnection :: WS.Connection
  , lsSessionId :: Maybe Text
  , lsControlUrl :: Text
  , lsSubscriptions :: TVar (Map Int LSSubscription)
  , lsNextSubId :: TVar Int
  , lsHeartbeatAsync :: Maybe (Async ())
  , lsMessageAsync :: Maybe (Async ())
  }

data LSSubscription = LSSubscription
  { lsSubId :: Int
  , lsItems :: [Text]          -- e.g., ["MARKET:CS.D.EURUSD.MINI.IP"]
  , lsFields :: [Text]         -- e.g., ["BID", "OFR", "UTM"]
  , lsInstrument :: Instrument
  , lsTickBuffer :: TVar [Tick]
  }

data LSMessage
  = LSControlResponse Text [(Text, Text)]
  | LSUpdateMessage Int [Maybe Text]  -- subscription id, field values
  | LSPing
  | LSLoop
  | LSError Text
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
  streamLogInfo "Establishing Lightstreamer WebSocket connection"

  -- TEMPORARY: Disable WebSocket streaming until TLCP protocol is properly implemented
  -- The current implementation has issues with the Lightstreamer handshake process
  streamLogWarn "WebSocket streaming temporarily disabled - using REST polling fallback"
  return $ Left $ brokerError "WebSocket streaming temporarily disabled - falling back to REST polling"

  {- DISABLED CODE - TO BE RE-ENABLED ONCE TLCP PROTOCOL IS FIXED
  case igLightstreamerEndpoint session of
    Nothing -> do
      streamLogError "No Lightstreamer endpoint in IG session"
      return $ Left $ brokerError "No Lightstreamer endpoint available"

    Just lsEndpoint -> do
      streamLogInfo $ "Connecting to Lightstreamer: " <> lsEndpoint

      -- Parse the endpoint URL
      let wsUrl = if "https://" `T.isPrefixOf` lsEndpoint
                  then T.replace "https://" "wss://" lsEndpoint <> "/lightstreamer"
                  else lsEndpoint <> "/lightstreamer"

      streamLogDebug $ "WebSocket URL: " <> wsUrl

      -- Establish WebSocket connection
      result <- try $ connectToLightstreamer wsUrl
      case result of
        Left (ex :: SomeException) -> do
          streamLogError $ "WebSocket connection failed: " <> T.pack (show ex)
          return $ Left $ brokerError $ "WebSocket connection failed: " <> T.pack (show ex)

        Right conn -> do
          streamLogInfo "WebSocket connection established"

          -- Initialize Lightstreamer connection state
          subscriptions <- newTVarIO Map.empty
          nextSubId <- newTVarIO 1

          let lsConn = LSConnection
                { lsConnection = conn
                , lsSessionId = Nothing
                , lsControlUrl = lsEndpoint
                , lsSubscriptions = subscriptions
                , lsNextSubId = nextSubId
                , lsHeartbeatAsync = Nothing
                , lsMessageAsync = Nothing
                }

          -- Create Lightstreamer session
          sessionResult <- createLightstreamerSession lsConn session
          case sessionResult of
            Left err -> do
              streamLogError $ "Failed to create Lightstreamer session: " <> T.pack (show err)
              return $ Left err

            Right sessionId -> do
              streamLogInfo $ "Lightstreamer session created: " <> sessionId

              let connWithSession = lsConn { lsSessionId = Just sessionId }

              -- Start message handler
              msgAsync <- async $ handleLightstreamerMessages connWithSession

              let finalConn = connWithSession { lsMessageAsync = Just msgAsync }

              streamLogInfo "Lightstreamer connection ready"
              return $ Right finalConn
  -}

-- Connect to Lightstreamer WebSocket
connectToLightstreamer :: Text -> IO WS.Connection
connectToLightstreamer wsUrl = withSocketsDo $ do
  let url = T.unpack wsUrl
      (host, path) = case T.splitOn "/" $ T.drop 6 wsUrl of  -- Remove "wss://"
        (h:pathParts) -> (T.unpack h, "/" <> T.unpack (T.intercalate "/" pathParts))
        [] -> error "Invalid WebSocket URL"

  streamLogDebug $ "Connecting to host: " <> T.pack host <> ", path: " <> T.pack path
  Wuss.runSecureClient host 443 path $ \conn -> do
    streamLogDebug "WebSocket handshake completed"
    return conn

-- Create Lightstreamer session
createLightstreamerSession :: LSConnection -> IGSession -> IO (Result Text)
createLightstreamerSession lsConn igSession = do
  streamLogInfo "Creating Lightstreamer session"

  -- Generate a unique connection ID
  connId <- UUID.nextRandom
  let connectionId = T.pack $ show connId

  -- Create session message
  let createMsg = formatControlMessage $ CreateSession (lsControlUrl lsConn) "QUOTE_ADAPTER"

  streamLogDebug $ "Sending create session: " <> createMsg

  -- Send create session message
  WS.sendTextData (lsConnection lsConn) createMsg

  -- Wait for session response
  response <- WS.receiveData (lsConnection lsConn)
  let responseText = TE.decodeUtf8 $ LBS.toStrict response

  streamLogDebug $ "Received response: " <> responseText

  case parseLightstreamerMessage responseText of
    LSControlResponse "CONOK" params -> do
      let sessionId = fromMaybe "" $ lookup "session_id" params
      if T.null sessionId
        then do
          streamLogError "No session ID in CONOK response"
          return $ Left $ brokerError "No session ID in create session response"
        else do
          streamLogInfo $ "Session created successfully: " <> sessionId

          -- Now bind the session
          bindResult <- bindLightstreamerSession lsConn sessionId connectionId
          case bindResult of
            Left err -> return $ Left err
            Right _ -> return $ Right sessionId

    LSError errMsg -> do
      streamLogError $ "Session creation failed: " <> errMsg
      return $ Left $ brokerError $ "Session creation failed: " <> errMsg

    _ -> do
      streamLogError $ "Unexpected response: " <> responseText
      return $ Left $ brokerError $ "Unexpected session creation response: " <> responseText

-- Bind Lightstreamer session
bindLightstreamerSession :: LSConnection -> Text -> Text -> IO (Result ())
bindLightstreamerSession lsConn sessionId connectionId = do
  streamLogInfo "Binding Lightstreamer session"

  let bindMsg = formatControlMessage $ BindSession sessionId connectionId

  streamLogDebug $ "Sending bind session: " <> bindMsg
  WS.sendTextData (lsConnection lsConn) bindMsg

  -- Wait for bind response
  response <- WS.receiveData (lsConnection lsConn)
  let responseText = TE.decodeUtf8 $ LBS.toStrict response

  streamLogDebug $ "Bind response: " <> responseText

  case parseLightstreamerMessage responseText of
    LSControlResponse "CONOK" _ -> do
      streamLogInfo "Session bound successfully"
      return $ Right ()

    LSError errMsg -> do
      streamLogError $ "Session binding failed: " <> errMsg
      return $ Left $ brokerError $ "Session binding failed: " <> errMsg

    _ -> do
      streamLogError $ "Unexpected bind response: " <> responseText
      return $ Left $ brokerError $ "Unexpected session bind response: " <> responseText

-- Subscribe to price updates for an instrument
subscribeToPriceUpdates :: LSConnection -> Instrument -> TVar [Tick] -> IO (Result LSSubscription)
subscribeToPriceUpdates lsConn instrument tickBuffer = do
  streamLogInfo $ "Subscribing to price updates for: " <> T.pack (show instrument)

  -- Convert instrument to IG market epic
  case instrumentToIGEpic instrument of
    Nothing -> do
      streamLogError $ "Unsupported instrument for streaming: " <> T.pack (show instrument)
      return $ Left $ brokerError $ "Unsupported instrument for streaming: " <> T.pack (show instrument)

    Just epic -> do
      -- Get next subscription ID
      subId <- atomically $ do
        current <- readTVar (lsNextSubId lsConn)
        writeTVar (lsNextSubId lsConn) (current + 1)
        return current

      let item = "MARKET:" <> epic
          fields = ["BID", "OFR", "UTM"]  -- Bid, Offer, Update Time

      streamLogDebug $ "Subscribing to item: " <> item <> " with fields: " <> T.intercalate ", " fields

      -- Create subscription record
      let subscription = LSSubscription
            { lsSubId = subId
            , lsItems = [item]
            , lsFields = fields
            , lsInstrument = instrument
            , lsTickBuffer = tickBuffer
            }

      -- Add to subscriptions map
      atomically $ modifyTVar (lsSubscriptions lsConn) $ Map.insert subId subscription

      -- Send subscription message
      let subMsg = formatControlMessage $ Subscribe [item] fields "MERGE"

      streamLogDebug $ "Sending subscription: " <> subMsg
      WS.sendTextData (lsConnection lsConn) subMsg

      streamLogInfo $ "Subscription created with ID: " <> T.pack (show subId)
      return $ Right subscription

-- Convert instrument to IG epic format
instrumentToIGEpic :: Instrument -> Maybe Text
instrumentToIGEpic (Instrument "EURUSD") = Just "CS.D.EURUSD.MINI.IP"
instrumentToIGEpic (Instrument "GBPUSD") = Just "CS.D.GBPUSD.MINI.IP"
instrumentToIGEpic (Instrument "USDJPY") = Just "CS.D.USDJPY.MINI.IP"
instrumentToIGEpic (Instrument "AUDUSD") = Just "CS.D.AUDUSD.MINI.IP"
instrumentToIGEpic (Instrument "USDCAD") = Just "CS.D.USDCAD.MINI.IP"
instrumentToIGEpic _ = Nothing  -- No fallback, return Nothing for unsupported instruments

-- Handle incoming Lightstreamer messages
handleLightstreamerMessages :: LSConnection -> IO ()
handleLightstreamerMessages lsConn = do
  streamLogInfo "Starting Lightstreamer message handler"

  let handleMessage = do
        response <- WS.receiveData (lsConnection lsConn)
        let responseText = TE.decodeUtf8 $ LBS.toStrict response

        when (not $ T.null $ T.strip responseText) $ do
          streamLogDebug $ "Received: " <> responseText

          case parseLightstreamerMessage responseText of
            LSUpdateMessage subId values -> do
              streamLogDebug $ "Processing update for subscription " <> T.pack (show subId)
              handleStreamingTick lsConn subId values

            LSPing -> do
              streamLogDebug "Received ping, sending pong"
              WS.sendTextData (lsConnection lsConn) ("PONG" :: Text)

            LSLoop -> do
              streamLogDebug "Received loop message"

            LSError errMsg -> do
              streamLogError $ "Received error: " <> errMsg

            msg -> do
              streamLogDebug $ "Received other message: " <> T.pack (show msg)

  result <- try $ forever handleMessage
  case result of
    Left (ex :: SomeException) -> do
      streamLogError $ "Message handler error: " <> T.pack (show ex)
    Right _ -> do
      streamLogInfo "Message handler completed"

-- Handle streaming tick data
handleStreamingTick :: LSConnection -> Int -> [Maybe Text] -> IO ()
handleStreamingTick lsConn subId values = do
  subs <- readTVarIO (lsSubscriptions lsConn)
  case Map.lookup subId subs of
    Nothing -> do
      streamLogWarn $ "Received tick for unknown subscription: " <> T.pack (show subId)

    Just subscription -> do
      streamLogDebug $ "Processing tick for " <> T.pack (show $ lsInstrument subscription)

      -- Extract bid, offer, and update time from values
      -- Values correspond to ["BID", "OFR", "UTM"] fields
      case values of
        [bidMaybe, offerMaybe, timeMaybe] -> do
          currentTime <- getCurrentTime

          let bid = bidMaybe >>= parseScientific >>= Just . realToFrac
              offer = offerMaybe >>= parseScientific >>= Just . realToFrac
              tickTime = fromMaybe currentTime $
                         timeMaybe >>= parseIGTime

          case (bid, offer) of
            (Just bidPrice, Just offerPrice) -> do
              let tick = Tick
                    { tTime = tickTime
                    , tInstr = lsInstrument subscription
                    , tBid = Price bidPrice
                    , tAsk = Price offerPrice
                    , tVolume = Nothing
                    }

              streamLogDebug $ "Created tick: " <> T.pack (show tick)

              -- Add tick to buffer
              atomically $ modifyTVar (lsTickBuffer subscription) (++ [tick])

            _ -> do
              streamLogWarn $ "Invalid price data: bid=" <> T.pack (show bid) <> ", offer=" <> T.pack (show offer)

        _ -> do
          streamLogWarn $ "Unexpected number of field values: " <> T.pack (show $ length values)

-- Parse scientific number from text
parseScientific :: Text -> Maybe Scientific
parseScientific t = case reads (T.unpack t) of
  [(val, "")] -> Just val
  _ -> Nothing

-- Parse IG timestamp format
parseIGTime :: Text -> Maybe UTCTime
parseIGTime timeStr = parseTimeM True defaultTimeLocale "%s%Q" (T.unpack timeStr)

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

  -- Close WebSocket connection

  streamLogInfo "Lightstreamer connection closed"

-- Format control messages for Lightstreamer protocol
formatControlMessage :: LSControlMessage -> Text
formatControlMessage msg = case msg of
  CreateSession controlUrl adapterSet ->
    "create_session\r\nLS_adapter_set=" <> adapterSet <>
    "&LS_cid=mgQkwtwdysogQz2BJ4Ji%20kOj2Bg&" <>
    "LS_send_sync=false&LS_cause=api\r\n"

  BindSession sessionId connectionId ->
    "bind_session\r\nLS_session=" <> sessionId <>
    "&LS_keepalive_millis=5000&LS_send_sync=false&" <>
    "LS_cause=ws.loop\r\n"

  Subscribe items fields mode ->
    let itemsStr = T.intercalate " " items
        fieldsStr = T.intercalate " " fields
    in "control\r\nLS_reqId=1&LS_op=add&LS_mode=" <> mode <>
       "&LS_group=" <> itemsStr <> "&LS_schema=" <> fieldsStr <> "\r\n"

  Unsubscribe subId ->
    "control\r\nLS_reqId=" <> T.pack (show subId) <> "&LS_op=delete\r\n"

-- Parse control response messages
parseControlResponse :: Text -> LSMessage
parseControlResponse msg =
  let -- Handle the CONOK format: "CONOK,session_id=value&other=value"
      afterConok = T.drop 5 msg  -- Remove "CONOK"
      allParts = if T.isPrefixOf "," afterConok
                 then T.drop 1 afterConok  -- Remove the comma after CONOK
                 else afterConok
      parts = T.splitOn "&" allParts
      params = map parseParam $ filter (T.isInfixOf "=") parts
  in LSControlResponse "CONOK" params
  where
    parseParam p = case T.splitOn "=" p of
      [key, value] -> (key, value)
      _ -> ("", "")

-- Parse Lightstreamer messages
parseLightstreamerMessage :: Text -> LSMessage
parseLightstreamerMessage msg
  | "CONOK" `T.isPrefixOf` msg = parseControlResponse msg
  | "CONERR" `T.isPrefixOf` msg = LSError $ T.drop 6 msg
  | "ERROR" `T.isPrefixOf` msg = LSError $ T.drop 5 msg
  | "PING" == T.strip msg = LSPing
  | "LOOP" `T.isPrefixOf` msg = LSLoop
  | isUpdateMessage msg = parseUpdateMessage msg
  | otherwise = LSError $ "Unknown message: " <> msg
  where
    -- Check if message looks like an update message (subId|field1|field2|...)
    isUpdateMessage m =
      case T.splitOn "|" m of
        (firstPart:_) -> case reads (T.unpack firstPart) of
          [(subId :: Int, "")] -> subId > 0  -- Valid positive subscription ID
          _ -> False
        _ -> False

-- Parse update messages (format: subId|field1|field2|...)
parseUpdateMessage :: Text -> LSMessage
parseUpdateMessage msg =
  case T.splitOn "|" msg of
    (subIdStr:values) ->
      case reads (T.unpack subIdStr) of
        [(subId, "")] ->
          let fieldValues = map (\v -> if T.null v then Nothing else Just v) values
          in LSUpdateMessage subId fieldValues
        _ -> LSError $ "Invalid subscription ID: " <> subIdStr
    _ -> LSError $ "Invalid update message format: " <> msg
