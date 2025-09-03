{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Adapter.BrokerDataProvider
  ( -- Types
    BrokerDataProviderM
  , BrokerConnection
  , IGMarket(..)
    -- Functions
  , runBrokerDataProviderIO
  , instrumentToIGEpic
  , convertIGMarketToTick
  ) where

import Domain.Services.LiveDataService
import Domain.Types
import Util.Config (BrokerConfig(..), BrokerType(..), BrokerEnvironment(..), ReconnectPolicy(..), AppConfig(..), acBroker, loadAppConfig)
import Util.Error (Result, TempehError(..), BrokerErrorDetails(..), brokerError)
import Control.Monad.Reader
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.STM (STM, TVar, newTVarIO, newTVar, readTVar, writeTVar, atomically)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent (threadDelay)
import Control.Exception (try, SomeException)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Data.Map (Map)
import qualified Data.Map as Map
import Network.HTTP.Conduit
import qualified Network.HTTP.Conduit as HTTP
import Network.HTTP.Types.Status (statusCode)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), (.:?), object, withObject, Value(..))
import qualified Data.Aeson as JSON
import Data.Aeson.Types (Parser)
import Data.Scientific (Scientific)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Control.Applicative ((<|>))
import Text.Read (readMaybe)

-- IG API Types
data IGSession = IGSession
  { igSessionToken :: Text
  , igCST :: Text  -- Client Security Token
  , igXSecurityToken :: Text
  , igExpiresAt :: UTCTime
  } deriving (Show)

data IGLoginRequest = IGLoginRequest
  { loginIdentifier :: Text
  , loginPassword :: Text
  , loginEncryptedPassword :: Bool
  } deriving (Show)

instance ToJSON IGLoginRequest where
  toJSON (IGLoginRequest ident pass encrypted) = object
    [ "identifier" .= ident
    , "password" .= pass
    , "encryptedPassword" .= encrypted
    ]

data IGLoginResponse = IGLoginResponse
  { responseAccountType :: Text
  , responseClientId :: Text
  , responseLightstreamerEndpoint :: Maybe Text
  } deriving (Show)

instance FromJSON IGLoginResponse where
  parseJSON = withObject "IGLoginResponse" $ \v -> IGLoginResponse
    <$> v .: "accountType"
    <*> v .: "clientId"
    <*> v .: "lightstreamerEndpoint"

data IGMarket = IGMarket
  { marketEpic :: Text
  , marketInstrument :: Text
  , marketBid :: Maybe Scientific
  , marketAsk :: Maybe Scientific
  , marketUpdateTime :: Maybe Text
  } deriving (Show)

instance FromJSON IGMarket where
  parseJSON = withObject "IGMarket" $ \v -> do
    -- Try flat structure first
    let parseFlat = do
          epic <- v .: "epic"
          name <- v .: "instrumentName"
          bidVal <- v .:? "bid"
          askVal <- v .:? "offer"
          upd <- v .:? "updateTime"
          bid <- parseSci bidVal
          ask <- parseSci askVal
          pure (IGMarket epic name bid ask upd)

    -- Try nested { instrument: {...}, snapshot: {...} }
    let parseNested = do
          inst <- v .: "instrument"
          snap <- v .: "snapshot"
          epic <- inst .: "epic"
          name <- inst .: "name" <|> inst .: "instrumentName"
          bidVal <- snap .:? "bid"
          askVal <- snap .:? "offer" <|> snap .:? "ask"
          upd <- snap .:? "updateTime"
          bid <- parseSci bidVal
          ask <- parseSci askVal
          pure (IGMarket epic name bid ask upd)

    parseFlat <|> parseNested
    where
      parseSci :: Maybe Value -> Parser (Maybe Scientific)
      parseSci Nothing = pure Nothing
      parseSci (Just (Number n)) = pure (Just n)
      parseSci (Just (String t)) =
        case readMaybe (T.unpack t) :: Maybe Double of
          Just d -> pure (Just (realToFrac d))
          Nothing -> pure Nothing
      parseSci _ = pure Nothing

-- Broker connection state
data BrokerConnection = BrokerConnection
  { bcConnectionId :: ConnectionId
  , bcConfig :: BrokerConfig
  , bcStatus :: TVar ConnectionStatus
  , bcLastHeartbeat :: TVar UTCTime
  , bcSubscriptions :: TVar (Map Instrument (TVar [Tick]))
  , bcReconnectCount :: TVar Int
  , bcHeartbeatAsync :: Maybe (Async ())
  , bcIGSession :: TVar (Maybe IGSession)  -- NEW: IG session management
  }

-- Broker adapter implementation
newtype BrokerDataProviderM m a = BrokerDataProviderM
  { runBrokerDataProvider :: ReaderT (TVar (Map ConnectionId BrokerConnection)) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (TVar (Map ConnectionId BrokerConnection)))

-- LiveDataProvider implementation for broker
instance (MonadIO m) => LiveDataProvider (BrokerDataProviderM m) where
  connect = do
    liftIO $ putStrLn "[BROKER] Attempting to connect to IG broker"
    connId <- liftIO $ ConnectionId . T.pack . show <$> getCurrentTime
    now <- liftIO getCurrentTime

    -- Load configuration to determine broker type
    configResult <- liftIO $ loadAppConfig
    case configResult of
      Left configErr -> do
        liftIO $ putStrLn $ "[BROKER] Failed to load config: " <> T.unpack configErr
        return $ Left $ brokerError configErr
      Right appConfig -> do
        let brokerConfig = acBroker appConfig

        case bcBrokerType brokerConfig of
          IG -> connectToIG connId brokerConfig now
          Demo -> connectDemo connId now
          _ -> do
            liftIO $ putStrLn "[BROKER] Broker type not yet implemented, using demo mode"
            connectDemo connId now

  disconnect connId = do
    liftIO $ putStrLn $ "[BROKER] Disconnecting from broker: " <> T.unpack (T.pack (show connId))
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
              Left err -> liftIO $ putStrLn $ "[BROKER] Logout failed: " <> show err
              Right _ -> liftIO $ putStrLn "[BROKER] Successfully logged out from IG"
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
    liftIO $ putStrLn $ "[BROKER] Subscribing to instrument: " <> T.unpack (T.pack (show instrument))
    connectionsVar <- ask
    result <- liftIO $ atomically $ do
      connections <- readTVar connectionsVar
      case Map.lookup connId connections of
        Nothing -> return $ Left $ brokerError ("Connection not found: " <> T.pack (show connId))
        Just conn -> do
          subs <- readTVar (bcSubscriptions conn)
          if Map.member instrument subs
            then return $ Right ()
            else do
              tickBuffer <- newTVar []
              let newSubs = Map.insert instrument tickBuffer subs
              writeTVar (bcSubscriptions conn) newSubs
              return $ Right ()

    case result of
      Left err -> return $ Left err
      Right _ -> do
        -- Start IG market data subscription
        startIGMarketDataSubscription connId instrument
        return $ Right ()

  unsubscribeFromInstrument connId instrument = do
    liftIO $ putStrLn $ "[BROKER] Unsubscribing from instrument: " <> T.unpack (T.pack (show instrument))
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
            Just tickBuffer -> do
              -- Return an STM action that atomically reads and clears the buffer
              let flush = do
                    xs <- readTVar tickBuffer
                    writeTVar tickBuffer []
                    return xs
              return $ Right flush

  getDataQuality connId instrument = do
    now <- liftIO getCurrentTime
    let quality = LiveDataQuality
          { ldqTicksReceived = 100
          , ldqTicksExpected = 100
          , ldqLatency = Just 15.5
          , ldqLastTickTime = Just now
          , ldqQualityScore = 0.98
          }
    return $ Right quality

-- IG-specific connection logic
connectToIG :: MonadIO m => ConnectionId -> BrokerConfig -> UTCTime -> BrokerDataProviderM m (Result ConnectionId)
connectToIG connId config now = do
  liftIO $ putStrLn "[BROKER] Connecting to IG using REST API"

  case (bcUsername config, bcPassword config, bcApiKey config) of
    (Just username, Just password, Just apiKey) -> do
      -- Attempt login to IG
      loginResult <- liftIO $ loginToIG config username password
      case loginResult of
        Left err -> do
          liftIO $ putStrLn $ "[BROKER] IG login failed: " <> show err
          return $ Left err
        Right session -> do
          liftIO $ putStrLn "[BROKER] Successfully authenticated with IG"

          -- Create connection with IG session
          statusVar <- liftIO $ newTVarIO $ Connected now
          heartbeatVar <- liftIO $ newTVarIO now
          subsVar <- liftIO $ newTVarIO Map.empty
          reconnectVar <- liftIO $ newTVarIO 0
          sessionVar <- liftIO $ newTVarIO $ Just session

          let connection = BrokerConnection
                { bcConnectionId = connId
                , bcConfig = config
                , bcStatus = statusVar
                , bcLastHeartbeat = heartbeatVar
                , bcSubscriptions = subsVar
                , bcReconnectCount = reconnectVar
                , bcHeartbeatAsync = Nothing
                , bcIGSession = sessionVar
                }

          -- Store connection
          connectionsVar <- ask
          liftIO $ atomically $ do
            connections <- readTVar connectionsVar
            writeTVar connectionsVar (Map.insert connId connection connections)

          return $ Right connId

    _ -> do
      liftIO $ putStrLn "[BROKER] Missing IG credentials, falling back to demo mode"
      connectDemo connId now

-- Demo connection (fallback)
connectDemo :: MonadIO m => ConnectionId -> UTCTime -> BrokerDataProviderM m (Result ConnectionId)
connectDemo connId now = do
  liftIO $ putStrLn "[BROKER] Creating demo connection"

  statusVar <- liftIO $ newTVarIO $ Connected now
  heartbeatVar <- liftIO $ newTVarIO now
  subsVar <- liftIO $ newTVarIO Map.empty
  reconnectVar <- liftIO $ newTVarIO 0
  sessionVar <- liftIO $ newTVarIO Nothing

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
        }

  connectionsVar <- ask
  liftIO $ atomically $ do
    connections <- readTVar connectionsVar
    writeTVar connectionsVar (Map.insert connId connection connections)

  return $ Right connId

-- IG REST API functions
loginToIG :: BrokerConfig -> Text -> Text -> IO (Result IGSession)
loginToIG config username password = do
  putStrLn "[BROKER] Authenticating with IG REST API"

  case bcBaseUrl config of
    Nothing -> return $ Left $ brokerError "No base URL configured for IG"
    Just baseUrl -> do
      let loginUrl = T.unpack baseUrl <> "/session"
          loginReq = IGLoginRequest username password False

      request <- parseRequest $ "POST " <> loginUrl
      let requestWithHeaders = request
            { requestHeaders =
                [ ("Content-Type", "application/json")
                , ("Accept", "application/json")
                , ("Version", "2")
                , ("X-IG-API-KEY", TE.encodeUtf8 $ maybe "" id (bcApiKey config))
                ]
            , requestBody = RequestBodyLBS $ JSON.encode loginReq
            }

      result <- try $ httpLbs requestWithHeaders =<< newManager tlsManagerSettings
      case result of
        Left (ex :: SomeException) -> do
          putStrLn $ "[BROKER] HTTP request failed: " <> show ex
          return $ Left $ brokerError ("HTTP request failed: " <> T.pack (show ex))

        Right response -> do
          let status = HTTP.responseStatus response
              headers = HTTP.responseHeaders response
              body = HTTP.responseBody response

          putStrLn $ "[BROKER] IG login response status: " <> show status

          if statusCode status == 200
            then do
              -- Extract session tokens from headers
              let cstHeader = lookup "CST" headers
                  xSecTokenHeader = lookup "X-SECURITY-TOKEN" headers

              case (cstHeader, xSecTokenHeader) of
                (Just cst, Just xSecToken) -> do
                  now <- getCurrentTime
                  let expiresAt = addUTCTime 21600 now  -- 6 hours from now
                      session = IGSession
                        { igSessionToken = TE.decodeUtf8 cst
                        , igCST = TE.decodeUtf8 cst
                        , igXSecurityToken = TE.decodeUtf8 xSecToken
                        , igExpiresAt = expiresAt
                        }

                  putStrLn "[BROKER] IG session tokens extracted successfully"
                  return $ Right session

                _ -> do
                  putStrLn "[BROKER] Missing session tokens in IG response headers"
                  return $ Left $ brokerError "Missing session tokens in IG response"

            else do
              putStrLn $ "[BROKER] IG login failed with status: " <> show status
              putStrLn $ "[BROKER] Response body: " <> show (LBS.take 500 body)
              return $ Left $ brokerError ("IG login failed with status: " <> T.pack (show status))

logoutFromIG :: BrokerConfig -> IGSession -> IO (Result ())
logoutFromIG config session = do
  putStrLn "[BROKER] Logging out from IG"

  case bcBaseUrl config of
    Nothing -> return $ Right ()  -- Nothing to logout from
    Just baseUrl -> do
      let logoutUrl = T.unpack baseUrl <> "/session"

      request <- parseRequest $ "DELETE " <> logoutUrl
      let requestWithHeaders = request
            { requestHeaders =
                [ ("Accept", "application/json")
                , ("Version", "1")
                , ("X-IG-API-KEY", TE.encodeUtf8 $ maybe "" id (bcApiKey config))
                , ("CST", TE.encodeUtf8 $ igCST session)
                , ("X-SECURITY-TOKEN", TE.encodeUtf8 $ igXSecurityToken session)
                ]
            }

      result <- try $ httpLbs requestWithHeaders =<< newManager tlsManagerSettings
      case result of
        Left (ex :: SomeException) -> do
          putStrLn $ "[BROKER] Logout request failed: " <> show ex
          return $ Right ()  -- Don't fail on logout errors

        Right response -> do
          let statusCode = HTTP.responseStatus response
          putStrLn $ "[BROKER] IG logout response status: " <> show statusCode
          return $ Right ()

-- IG market data subscription (placeholder for now)
startIGMarketDataSubscription :: MonadIO m => ConnectionId -> Instrument -> BrokerDataProviderM m ()
startIGMarketDataSubscription connId instrument = do
  liftIO $ putStrLn $ "[BROKER] Starting IG market data subscription for " <> T.unpack (T.pack (show instrument))

  -- Get connection to access IG session
  connectionsVar <- ask
  maybeConn <- liftIO $ atomically $ do
    connections <- readTVar connectionsVar
    return $ Map.lookup connId connections

  case maybeConn of
    Nothing -> liftIO $ putStrLn "[BROKER] Connection not found for market data subscription"
    Just conn -> do
      maybeSession <- liftIO $ atomically $ readTVar (bcIGSession conn)
      case maybeSession of
        Nothing -> liftIO $ putStrLn "[BROKER] No IG session available for market data"
        Just session -> do
          -- Start background tick polling for this instrument
          _ <- liftIO $ async $ pollIGMarketDataLoop conn session instrument
          liftIO $ putStrLn $ "[BROKER] Started market data polling for " <> T.unpack (T.pack (show instrument))

-- Background loop to poll IG market data
pollIGMarketDataLoop :: BrokerConnection -> IGSession -> Instrument -> IO ()
pollIGMarketDataLoop conn session instrument = do
  putStrLn $ "[BROKER] Starting market data polling loop for " <> show instrument
  pollLoop
  where
    pollLoop = do
      -- Poll market data from IG using the broker config (includes API key)
      result <- pollIGMarketData (bcConfig conn) session instrument
      case result of
        Left err -> do
          putStrLn $ "[BROKER] Market data polling error: " <> show err
          threadDelay 5000000  -- Wait 5 seconds before retry
          pollLoop
        Right ticks -> do
          -- Update tick buffer
          atomically $ do
            subs <- readTVar (bcSubscriptions conn)
            case Map.lookup instrument subs of
              Nothing -> return ()  -- Not subscribed anymore
              Just tickBuffer -> do
                currentTicks <- readTVar tickBuffer
                writeTVar tickBuffer (ticks ++ currentTicks)

          -- Wait before next poll (1 second for demo, could be faster for live)
          threadDelay 1000000  -- 1 second
          pollLoop

-- Poll current market data from IG REST API
pollIGMarketData :: BrokerConfig -> IGSession -> Instrument -> IO (Result [Tick])
pollIGMarketData config session instrument = do
  case instrumentToIGEpic instrument of
    Nothing -> return $ Left $ brokerError ("Unsupported instrument for IG: " <> T.pack (show instrument))
    Just epic -> do
      case bcBaseUrl config of
        Nothing -> return $ Left $ brokerError "No base URL configured for IG"
        Just baseUrl -> do
          let marketUrl = T.unpack baseUrl <> "/markets/" <> T.unpack epic

          request <- parseRequest $ "GET " <> marketUrl
          let requestWithHeaders = request
                { requestHeaders =
                    [ ("Accept", "application/json")
                    , ("Version", "3")
                    , ("X-IG-API-KEY", TE.encodeUtf8 $ maybe "" id (bcApiKey config))
                    , ("CST", TE.encodeUtf8 $ igCST session)
                    , ("X-SECURITY-TOKEN", TE.encodeUtf8 $ igXSecurityToken session)
                    ]
                }

          result <- try $ httpLbs requestWithHeaders =<< newManager tlsManagerSettings
          case result of
            Left (ex :: SomeException) -> do
              putStrLn $ "[BROKER] Market data request failed: " <> show ex
              return $ Left $ brokerError ("Market data request failed: " <> T.pack (show ex))

            Right response -> do
              let status = HTTP.responseStatus response
                  body = HTTP.responseBody response

              if statusCode status == 200
                then do
                  -- Parse market data response (supports both flat and nested forms)
                  case JSON.eitherDecode body of
                    Left _ -> do
                      putStrLn $ "[BROKER] Failed to parse market data response"
                      return $ Left $ brokerError "Failed to parse market data response"
                    Right marketData -> do
                      now <- getCurrentTime
                      let tick = convertIGMarketToTick marketData instrument now
                      return $ Right [tick]
                else do
                  putStrLn $ "[BROKER] Market data request failed with status: " <> show status
                  return $ Left $ brokerError ("Market data request failed with status: " <> T.pack (show status))

-- Convert IG market data to domain tick
convertIGMarketToTick :: IGMarket -> Instrument -> UTCTime -> Tick
convertIGMarketToTick igMarket instrument timestamp = Tick
  { tTime = timestamp
  , tInstr = instrument
  , tBid = Price $ maybe 0 id (marketBid igMarket)
  , tAsk = Price $ maybe 0 id (marketAsk igMarket)
  , tVolume = Nothing  -- IG doesn't provide volume in basic market data
  }

-- Map instruments to IG epics (market identifiers)
instrumentToIGEpic :: Instrument -> Maybe Text
instrumentToIGEpic (Instrument instr) = case instr of
  "EURUSD" -> Just "CS.D.EURUSD.CFD.IP"
  "GBPUSD" -> Just "CS.D.GBPUSD.CFD.IP"
  "USDJPY" -> Just "CS.D.USDJPY.CFD.IP"
  "AUDUSD" -> Just "CS.D.AUDUSD.CFD.IP"
  "USDCAD" -> Just "CS.D.USDCAD.CFD.IP"
  _ -> Nothing  -- Add more instruments as needed

-- Helper functions for managing polling threads
data PollingThread = PollingThread
  { ptInstrument :: Instrument
  , ptAsync :: Async ()
  }

-- Helper to run broker data provider
runBrokerDataProviderIO :: BrokerDataProviderM IO a -> IO a
runBrokerDataProviderIO action = do
  connectionsVar <- newTVarIO Map.empty
  runReaderT (runBrokerDataProvider action) connectionsVar
