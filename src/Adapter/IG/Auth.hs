{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Adapter.IG.Auth
  ( loginToIG
  , logoutFromIG
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime, addUTCTime)
import Network.HTTP.Conduit
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

import Adapter.IG.Types
import Domain.Types
import Util.Config (BrokerConfig(..))
import Util.Error (Result, brokerError)
import Util.Logger (ComponentLogger, makeComponentLogger)

-- Component logger for this module
authLogger :: ComponentLogger
authLogger = makeComponentLogger "IG_AUTH"

-- IG REST API functions
loginToIG :: BrokerConfig -> Text -> Text -> IO (Result IGSession)
loginToIG config username password = do
  compLogInfo authLogger ("Attempting login to IG for user: " <> username)

  case bcBaseUrl config of
    Nothing -> do
      compLogError authLogger "No base URL configured for IG"
      return $ Left $ brokerError "No base URL configured for IG"
    Just baseUrl -> do
      result <- try $ doLogin config baseUrl username password
      case result of
        Left (ex :: SomeException) -> do
          compLogError authLogger ("Login failed with exception: " <> T.pack (show ex))
          return $ Left $ brokerError ("Login failed: " <> T.pack (show ex))
        Right loginResult -> return loginResult

doLogin :: BrokerConfig -> Text -> Text -> Text -> IO (Result IGSession)
doLogin config baseUrl username password = do
  compLogDebug authLogger "Creating login request"

  -- Create request body
  let loginRequest = LoginRequest
        { identifier = username
        , password = password
        , encryptedPassword = False
        }

  -- Create HTTP request
  let url = T.unpack baseUrl <> "/session"
  request <- parseRequest $ "POST " <> url
  let requestWithHeaders = request
        { requestHeaders =
            [ ("Accept", "application/json; charset=UTF-8")
            , ("Content-Type", "application/json; charset=UTF-8")
            , ("X-IG-API-KEY", TE.encodeUtf8 $ maybe "" id (bcApiKey config))
            , ("Version", "2")
            ]
        , requestBody = RequestBodyLBS $ JSON.encode loginRequest
        }

  compLogDebug authLogger ("Sending login request to: " <> T.pack url)

  -- Send request
  response <- httpLbs requestWithHeaders =<< newManager tlsManagerSettings
  let status = responseStatus response
      body = responseBody response
      headers = responseHeaders response

  compLogDebug authLogger ("Login response status: " <> T.pack (show status))

  -- Debug response body (first 500 chars)
  let brokerLogDebugBody b = compLogDebug authLogger ("Response body: " <> T.pack (show (LBS.take 500 b)))
  brokerLogDebugBody body

  if statusCode status == 200
    then parseLoginResponse headers body
    else do
      compLogError authLogger ("Login failed with status: " <> T.pack (show status))
      return $ Left $ brokerError ("Login failed with status: " <> T.pack (show status))

logoutFromIG :: BrokerConfig -> IGSession -> IO (Result ())
logoutFromIG config session = do
  compLogInfo authLogger "Logging out from IG"

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
          compLogWarn authLogger ("Logout request failed: " <> T.pack (show ex))
          return $ Right ()
        Right response -> do
          let status = responseStatus response
          compLogInfo authLogger ("IG logout response status: " <> T.pack (show status))
          return $ Right ()
