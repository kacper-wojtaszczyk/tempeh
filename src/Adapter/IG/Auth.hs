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
import Util.Logger (logInfo, logError, logWarn, logDebug, ComponentName(..), runFileLoggerWithComponent)

-- Adapter-scoped logging helpers
brokerLogInfo :: Text -> IO ()
brokerLogInfo msg = runFileLoggerWithComponent (ComponentName "BROKER") $ logInfo msg

brokerLogWarn :: Text -> IO ()
brokerLogWarn msg = runFileLoggerWithComponent (ComponentName "BROKER") $ logWarn msg

brokerLogError :: Text -> IO ()
brokerLogError msg = runFileLoggerWithComponent (ComponentName "BROKER") $ logError msg

brokerLogDebug :: Text -> IO ()
brokerLogDebug msg = runFileLoggerWithComponent (ComponentName "BROKER") $ logDebug msg

-- IG REST API functions
loginToIG :: BrokerConfig -> Text -> Text -> IO (Result IGSession)
loginToIG config username password = do
  brokerLogInfo "Authenticating with IG REST API"

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
          brokerLogError ("HTTP request failed: " <> T.pack (show ex))
          return $ Left $ brokerError ("HTTP request failed: " <> T.pack (show ex))

        Right response -> do
          let status = responseStatus response
              headers = responseHeaders response
              body = responseBody response

          brokerLogInfo ("IG login response status: " <> T.pack (show status))

          if statusCode status == 200
            then do
              let cstHeader = lookup "CST" headers
                  xSecTokenHeader = lookup "X-SECURITY-TOKEN" headers
                  mParsedBody = JSON.decode body :: Maybe IGLoginResponse
              case (cstHeader, xSecTokenHeader) of
                (Just cst, Just xSecToken) -> do
                  now <- getCurrentTime
                  let expiresAt = addUTCTime 21600 now
                      session = IGSession
                        { igSessionToken = TE.decodeUtf8 cst
                        , igCST = TE.decodeUtf8 cst
                        , igXSecurityToken = TE.decodeUtf8 xSecToken
                        , igExpiresAt = expiresAt
                        , igLightstreamerEndpoint = responseLightstreamerEndpoint =<< mParsedBody
                        }
                  brokerLogInfo "IG session tokens extracted successfully"
                  return $ Right session
                _ -> do
                  brokerLogError "Missing session tokens in IG response headers"
                  return $ Left $ brokerError "Missing session tokens in IG response"
            else do
              brokerLogError ("IG login failed with status: " <> T.pack (show status))
              brokerLogDebugBody body
              return $ Left $ brokerError ("IG login failed with status: " <> T.pack (show status))
  where
    brokerLogDebugBody b = runFileLoggerWithComponent (ComponentName "BROKER") $ logDebug ("Response body: " <> T.pack (show (LBS.take 500 b)))

logoutFromIG :: BrokerConfig -> IGSession -> IO (Result ())
logoutFromIG config session = do
  brokerLogInfo "Logging out from IG"

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
          brokerLogWarn ("Logout request failed: " <> T.pack (show ex))
          return $ Right ()
        Right response -> do
          let status = responseStatus response
          brokerLogInfo ("IG logout response status: " <> T.pack (show status))
          return $ Right ()
