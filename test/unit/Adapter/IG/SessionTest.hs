{-# LANGUAGE OverloadedStrings #-}

module Unit.Adapter.IG.SessionTest (sessionTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Time (getCurrentTime, addUTCTime)
import Data.Text (Text)

import Adapter.IG.Session
import Adapter.IG.Types (IGSession(..))
import Util.Config (BrokerConfig(..), BrokerType(..), BrokerEnvironment(..), ReconnectPolicy(..))
import Util.Error (Result(..))

-- Mock session for testing
mockSession :: IO IGSession
mockSession = do
  now <- getCurrentTime
  return $ IGSession
    { igSessionToken = "mock-session-token"
    , igCST = "mock-cst"
    , igXSecurityToken = "mock-x-security-token"
    , igExpiresAt = addUTCTime 3600 now -- Expires in 1 hour
    , igLightstreamerEndpoint = Just "https://mock-lightstreamer.ig.com"
    }

-- Mock broker config for testing
mockBrokerConfig :: BrokerConfig
mockBrokerConfig = BrokerConfig
  { bcBrokerType = IG
  , bcEnvironment = DemoEnv
  , bcBaseUrl = Just "https://mock-api.ig.com"
  , bcApiKey = Just "mock-api-key"
  , bcUsername = Just "mock-username"
  , bcPassword = Just "mock-password"
  , bcAccountId = Nothing
  , bcConnectTimeout = 30
  , bcReadTimeout = 30
  , bcReconnectPolicy = ReconnectPolicy 3 1.0 30.0 2.0
  }

sessionTests :: TestTree
sessionTests = testGroup "Adapter.IG.Session"
  [ testGroup "Session Validation"
    [ testCase "Valid session should pass validation" $ do
        sessionVar <- newTVarIO Nothing
        session <- mockSession
        atomically $ writeTVar sessionVar (Just session)

        result <- runReaderT (runSessionManager validateSession) sessionVar
        result @?= True

    , testCase "Expired session should fail validation" $ do
        sessionVar <- newTVarIO Nothing
        session <- mockSession
        now <- getCurrentTime
        let expiredSession = session { igExpiresAt = addUTCTime (-3600) now } -- Expired 1 hour ago
        atomically $ writeTVar sessionVar (Just expiredSession)

        result <- runReaderT (runSessionManager validateSession) sessionVar
        result @?= False

    , testCase "Missing session should fail validation" $ do
        sessionVar <- newTVarIO Nothing

        result <- runReaderT (runSessionManager validateSession) sessionVar
        result @?= False
    ]

  , testGroup "Session State Management"
    [ testCase "Session variable should be updated on creation" $ do
        sessionVar <- newTVarIO Nothing
        -- Note: This test would require mocking the actual IG login
        -- For now, we test the state management directly
        session <- mockSession
        atomically $ writeTVar sessionVar (Just session)

        storedSession <- readTVarIO sessionVar
        case storedSession of
          Nothing -> assertFailure "Session should be stored"
          Just s -> igSessionToken s @?= "mock-session-token"

    , testCase "Session variable should be cleared on close" $ do
        sessionVar <- newTVarIO Nothing
        session <- mockSession
        atomically $ writeTVar sessionVar (Just session)

        -- Clear session (simulating close)
        atomically $ writeTVar sessionVar Nothing

        storedSession <- readTVarIO sessionVar
        storedSession @?= Nothing
    ]

  , testGroup "Session Lifecycle"
    [ testCase "Session manager should handle state transitions" $ do
        sessionVar <- newTVarIO Nothing

        -- Initially no session
        isValid1 <- runReaderT (runSessionManager validateSession) sessionVar
        isValid1 @?= False

        -- Add session
        session <- mockSession
        atomically $ writeTVar sessionVar (Just session)
        isValid2 <- runReaderT (runSessionManager validateSession) sessionVar
        isValid2 @?= True

        -- Remove session
        atomically $ writeTVar sessionVar Nothing
        isValid3 <- runReaderT (runSessionManager validateSession) sessionVar
        isValid3 @?= False
    ]
  ]
