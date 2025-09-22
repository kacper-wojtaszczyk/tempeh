{-# LANGUAGE OverloadedStrings #-}

module Unit.Adapter.IG.ConnectionTest (connectionTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Time (getCurrentTime)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import Adapter.IG.Connection
import Adapter.IG.Types (IGSession(..), BrokerConnection(..), SubscriptionState(..), StreamingMode(..))
import Domain.Types (ConnectionId(..), ConnectionStatus(..), Instrument(..))
import Util.Config (BrokerConfig(..), BrokerType(..), BrokerEnvironment(..), ReconnectPolicy(..))
import Util.Error (Result(..))

-- Mock data for testing
mockConnectionId :: ConnectionId
mockConnectionId = ConnectionId "test-conn-123"

-- Mock broker config for testing
mockBrokerConfig :: BrokerConfig
mockBrokerConfig = BrokerConfig
  { bcBrokerType = IG
  , bcEnvironment = DemoEnv
  , bcBaseUrl = Just "https://test-api.ig.com"
  , bcApiKey = Just "test-api-key"
  , bcUsername = Just "test-username"
  , bcPassword = Just "test-password"
  , bcAccountId = Nothing
  , bcConnectTimeout = 30
  , bcReadTimeout = 30
  , bcReconnectPolicy = ReconnectPolicy
    { rpMaxRetries = 3
    , rpInitialDelay = 1.0
    , rpMaxDelay = 30.0
    , rpBackoffMultiplier = 2.0
    }
  }

mockIGSession :: IO IGSession
mockIGSession = do
  now <- getCurrentTime
  return $ IGSession
    { igSessionToken = "test-session-token"
    , igCST = "test-cst"
    , igXSecurityToken = "test-x-security-token"
    , igExpiresAt = now
    , igLightstreamerEndpoint = Nothing
    }

connectionTests :: TestTree
connectionTests = testGroup "Adapter.IG.Connection"
  [ testGroup "Connection Management"
    [ testCase "Establish connection should create connection entry" $ do
        connectionsVar <- newTVarIO Map.empty
        session <- mockIGSession

        result <- runReaderT (runConnectionManager (establishConnection mockBrokerConfig mockConnectionId session)) connectionsVar

        case result of
          Left err -> assertFailure $ "Connection establishment failed: " ++ show err
          Right connId -> do
            connId @?= mockConnectionId
            connections <- readTVarIO connectionsVar
            Map.member mockConnectionId connections @?= True

    , testCase "Close connection should remove connection entry" $ do
        connectionsVar <- newTVarIO Map.empty
        session <- mockIGSession

        -- First establish connection
        _ <- runReaderT (runConnectionManager (establishConnection mockBrokerConfig mockConnectionId session)) connectionsVar

        -- Then close it
        result <- runReaderT (runConnectionManager (closeConnection mockConnectionId)) connectionsVar

        case result of
          Left err -> assertFailure $ "Connection close failed: " ++ show err
          Right () -> do
            connections <- readTVarIO connectionsVar
            Map.member mockConnectionId connections @?= False

    , testCase "Get connection state should return connection info" $ do
        connectionsVar <- newTVarIO Map.empty
        session <- mockIGSession

        -- Establish connection first
        _ <- runReaderT (runConnectionManager (establishConnection mockBrokerConfig mockConnectionId session)) connectionsVar

        -- Get connection state
        result <- runReaderT (runConnectionManager (getConnectionState mockConnectionId)) connectionsVar

        case result of
          Left err -> assertFailure $ "Get connection state failed: " ++ show err
          Right state -> csConnectionId state @?= mockConnectionId
    ]

  , testGroup "Connection ID Generation"
    [ testCase "Generate connection ID should return valid text" $ do
        connId <- generateConnectionId
        let connIdStr = T.unpack connId
        length connIdStr @?= 7 -- Should be 7 digits (1000000-9999999)
        all (\c -> c >= '0' && c <= '9') connIdStr @?= True -- Should be all digits
    ]

  , testGroup "Error Handling"
    [ testCase "Close non-existent connection should return error" $ do
        connectionsVar <- newTVarIO Map.empty

        result <- runReaderT (runConnectionManager (closeConnection mockConnectionId)) connectionsVar

        case result of
          Left _ -> return () -- Expected error
          Right () -> assertFailure "Should have failed for non-existent connection"

    , testCase "Get state of non-existent connection should return error" $ do
        connectionsVar <- newTVarIO Map.empty

        result <- runReaderT (runConnectionManager (getConnectionState mockConnectionId)) connectionsVar

        case result of
          Left _ -> return () -- Expected error
          Right _ -> assertFailure "Should have failed for non-existent connection"
    ]
  ]
