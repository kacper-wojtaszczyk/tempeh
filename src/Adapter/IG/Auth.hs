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
import qualified Network.HTTP.Types.Header
import qualified Data.CaseInsensitive as CI

import Adapter.IG.Types
import Domain.Types
import Util.Config (BrokerConfig(..))
import Util.Error (Result, brokerError)
import Util.Logger (ComponentLogger, makeComponentLogger, compLogInfo, compLogError, compLogDebug, compLogWarn)

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
  let loginRequest = IGLoginRequest
        { loginUsername = username
        , loginPassword = password
        , loginEncryptedPassword = False
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

-- Parse login response to extract session tokens
parseLoginResponse :: Network.HTTP.Types.Header.ResponseHeaders -> LBS.ByteString -> IO (Result IGSession)
parseLoginResponse headers body = do
  compLogDebug authLogger "Parsing login response"

  -- Extract CST and X-SECURITY-TOKEN from headers
  let cstToken = extractHeaderValue "CST" headers
      securityToken = extractHeaderValue "X-SECURITY-TOKEN" headers

  case (cstToken, securityToken) of
    (Just cst, Just security) -> do
      -- Parse JSON response for additional session info
      case JSON.decode body :: Maybe IGLoginResponse of
        Just loginResp -> do
          now <- getCurrentTime
          let session = IGSession
                { igSessionToken = cst  -- Use CST as session token
                , igCST = cst
                , igXSecurityToken = security
                , igLightstreamerEndpoint = responseLightstreamerEndpoint loginResp
                , igExpiresAt = addUTCTime 21600 now -- 6 hours from now
                }
          compLogInfo authLogger "Login successful"
          return $ Right session
        Nothing -> do
          compLogError authLogger "Failed to parse login response JSON"
          compLogDebug authLogger ("Response body: " <> T.pack (show (LBS.take 1000 body)))
          return $ Left $ brokerError "Failed to parse login response"
    _ -> do
      compLogError authLogger ("Missing session tokens in response headers. CST: " <> T.pack (show cstToken) <> ", Security: " <> T.pack (show securityToken))
      return $ Left $ brokerError "Missing session tokens in login response"

-- Helper function to extract header values
extractHeaderValue :: String -> Network.HTTP.Types.Header.ResponseHeaders -> Maybe Text
extractHeaderValue headerName headers =
  case lookup (fromString headerName) headers of
    Just value -> Just (TE.decodeUtf8 value)
    Nothing -> Nothing
  where
    fromString = CI.mk . TE.encodeUtf8 . T.pack

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
