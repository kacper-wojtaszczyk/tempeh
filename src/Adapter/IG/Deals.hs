{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Adapter.IG.Deals
  ( -- Position management
    getPositions
  , getPosition
  , createPosition
  , amendPosition
  , closePosition
  , closePositionById
  -- Working orders
  , getWorkingOrders
  , createWorkingOrder
  , amendWorkingOrder
  , deleteWorkingOrder
  -- Deal confirmations
  , getDealConfirmation
  ) where

import Control.Exception (try, SomeException)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Conduit
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

import Adapter.IG.Types
import Util.Config (BrokerConfig(..))
import Util.Error (Result, brokerError)
import Util.Logger (ComponentLogger, makeComponentLogger, compLogInfo, compLogError, compLogDebug, compLogWarn)

-- Component logger for this module
dealsLogger :: ComponentLogger
dealsLogger = makeComponentLogger "IG_DEALS"

-- Helper function to create authenticated requests
createAuthenticatedRequest :: BrokerConfig -> IGSession -> String -> String -> IO Request
createAuthenticatedRequest config session method path = do
  case bcBaseUrl config of
    Nothing -> error "No base URL configured for IG"
    Just baseUrl -> do
      let url = T.unpack baseUrl <> path
      request <- parseRequest $ method <> " " <> url
      return $ request
        { requestHeaders =
            [ ("Accept", "application/json; charset=UTF-8")
            , ("X-IG-API-KEY", TE.encodeUtf8 $ maybe "" id (bcApiKey config))
            , ("CST", TE.encodeUtf8 $ igCST session)
            , ("X-SECURITY-TOKEN", TE.encodeUtf8 $ igXSecurityToken session)
            ]
        }

-- Helper function for JSON POST requests
createJsonRequest :: BrokerConfig -> IGSession -> String -> String -> Text -> JSON.Value -> IO Request
createJsonRequest config session method path version body = do
  baseReq <- createAuthenticatedRequest config session method path
  return $ baseReq
    { requestHeaders = requestHeaders baseReq <>
        [ ("Content-Type", "application/json; charset=UTF-8")
        , ("Version", TE.encodeUtf8 version)
        ]
    , requestBody = RequestBodyLBS $ JSON.encode body
    }

-- Helper function for GET requests with version
createVersionedRequest :: BrokerConfig -> IGSession -> String -> Text -> IO Request
createVersionedRequest config session path version = do
  baseReq <- createAuthenticatedRequest config session "GET" path
  return $ baseReq
    { requestHeaders = requestHeaders baseReq <>
        [ ("Version", TE.encodeUtf8 version)
        ]
    }

-- Position Management Functions

-- Get all positions
getPositions :: BrokerConfig -> IGSession -> IO (Result [IGPosition])
getPositions config session = do
  compLogInfo dealsLogger "Fetching all positions"

  result <- try $ do
    request <- createVersionedRequest config session "/positions" "2"
    response <- httpLbs request =<< newManager tlsManagerSettings
    return response

  case result of
    Left (ex :: SomeException) -> do
      compLogError dealsLogger ("Failed to fetch positions: " <> T.pack (show ex))
      return $ Left $ brokerError ("Failed to fetch positions: " <> T.pack (show ex))

    Right response -> do
      let status = responseStatus response
          body = responseBody response

      compLogDebug dealsLogger ("Positions response status: " <> T.pack (show status))

      if statusCode status == 200
        then case JSON.decode body :: Maybe IGPositionResponse of
          Just posResp -> do
            compLogInfo dealsLogger ("Found " <> T.pack (show (length (positionsData posResp))) <> " positions")
            return $ Right $ positionsData posResp
          Nothing -> do
            compLogError dealsLogger "Failed to parse positions response"
            compLogDebug dealsLogger ("Response body: " <> T.pack (show (LBS.take 500 body)))
            return $ Left $ brokerError "Failed to parse positions response"
        else do
          compLogError dealsLogger ("Get positions failed with status: " <> T.pack (show status))
          return $ Left $ brokerError ("Get positions failed with status: " <> T.pack (show status))

-- Get single position by deal ID
getPosition :: BrokerConfig -> IGSession -> Text -> IO (Result IGPosition)
getPosition config session dealId = do
  compLogInfo dealsLogger ("Fetching position: " <> dealId)

  result <- try $ do
    request <- createVersionedRequest config session ("/positions/" <> T.unpack dealId) "2"
    response <- httpLbs request =<< newManager tlsManagerSettings
    return response

  case result of
    Left (ex :: SomeException) -> do
      compLogError dealsLogger ("Failed to fetch position: " <> T.pack (show ex))
      return $ Left $ brokerError ("Failed to fetch position: " <> T.pack (show ex))

    Right response -> do
      let status = responseStatus response
          body = responseBody response

      if statusCode status == 200
        then case JSON.decode body :: Maybe IGPosition of
          Just position -> do
            compLogInfo dealsLogger ("Retrieved position: " <> dealId)
            return $ Right position
          Nothing -> do
            compLogError dealsLogger "Failed to parse position response"
            return $ Left $ brokerError "Failed to parse position response"
        else do
          compLogError dealsLogger ("Get position failed with status: " <> T.pack (show status))
          return $ Left $ brokerError ("Get position failed with status: " <> T.pack (show status))

-- Create new position
createPosition :: BrokerConfig -> IGSession -> IGDealRequest -> IO (Result IGDealResponse)
createPosition config session dealReq = do
  compLogInfo dealsLogger ("Creating position: " <> dealEpic dealReq <> " " <> T.pack (show (dealDirection dealReq)) <> " " <> T.pack (show (dealSize dealReq)))

  result <- try $ do
    request <- createJsonRequest config session "POST" "/positions/otc" "2" (JSON.toJSON dealReq)
    response <- httpLbs request =<< newManager tlsManagerSettings
    return response

  case result of
    Left (ex :: SomeException) -> do
      compLogError dealsLogger ("Failed to create position: " <> T.pack (show ex))
      return $ Left $ brokerError ("Failed to create position: " <> T.pack (show ex))

    Right response -> do
      let status = responseStatus response
          body = responseBody response

      compLogDebug dealsLogger ("Position creation response status: " <> T.pack (show status))

      if statusCode status == 200
        then case JSON.decode body :: Maybe IGDealResponse of
          Just dealResp -> do
            compLogInfo dealsLogger ("Position creation request submitted: " <> dealResponseReference dealResp)
            return $ Right dealResp
          Nothing -> do
            compLogError dealsLogger "Failed to parse deal response"
            compLogDebug dealsLogger ("Response body: " <> T.pack (show (LBS.take 500 body)))
            return $ Left $ brokerError "Failed to parse deal response"
        else do
          compLogError dealsLogger ("Create position failed with status: " <> T.pack (show status))
          compLogDebug dealsLogger ("Response body: " <> T.pack (show (LBS.take 500 body)))
          return $ Left $ brokerError ("Create position failed with status: " <> T.pack (show status))

-- Amend existing position
amendPosition :: BrokerConfig -> IGSession -> Text -> IGDealRequest -> IO (Result IGDealResponse)
amendPosition config session dealId amendReq = do
  compLogInfo dealsLogger ("Amending position: " <> dealId)

  result <- try $ do
    request <- createJsonRequest config session "PUT" ("/positions/otc/" <> T.unpack dealId) "2" (JSON.toJSON amendReq)
    response <- httpLbs request =<< newManager tlsManagerSettings
    return response

  case result of
    Left (ex :: SomeException) -> do
      compLogError dealsLogger ("Failed to amend position: " <> T.pack (show ex))
      return $ Left $ brokerError ("Failed to amend position: " <> T.pack (show ex))

    Right response -> do
      let status = responseStatus response
          body = responseBody response

      if statusCode status == 200
        then case JSON.decode body :: Maybe IGDealResponse of
          Just dealResp -> do
            compLogInfo dealsLogger ("Position amendment submitted: " <> dealResponseReference dealResp)
            return $ Right dealResp
          Nothing -> do
            compLogError dealsLogger "Failed to parse amendment response"
            return $ Left $ brokerError "Failed to parse amendment response"
        else do
          compLogError dealsLogger ("Amend position failed with status: " <> T.pack (show status))
          return $ Left $ brokerError ("Amend position failed with status: " <> T.pack (show status))

-- Close position using DELETE /positions/otc with criteria
closePosition :: BrokerConfig -> IGSession -> IGDealRequest -> IO (Result IGDealResponse)
closePosition config session closeReq = do
  compLogInfo dealsLogger ("Closing position with criteria")

  result <- try $ do
    request <- createJsonRequest config session "DELETE" "/positions/otc" "1" (JSON.toJSON closeReq)
    response <- httpLbs request =<< newManager tlsManagerSettings
    return response

  case result of
    Left (ex :: SomeException) -> do
      compLogError dealsLogger ("Failed to close position: " <> T.pack (show ex))
      return $ Left $ brokerError ("Failed to close position: " <> T.pack (show ex))

    Right response -> do
      let status = responseStatus response
          body = responseBody response

      if statusCode status == 200
        then case JSON.decode body :: Maybe IGDealResponse of
          Just dealResp -> do
            compLogInfo dealsLogger ("Position close request submitted: " <> dealResponseReference dealResp)
            return $ Right dealResp
          Nothing -> do
            compLogError dealsLogger "Failed to parse close response"
            return $ Left $ brokerError "Failed to parse close response"
        else do
          compLogError dealsLogger ("Close position failed with status: " <> T.pack (show status))
          return $ Left $ brokerError ("Close position failed with status: " <> T.pack (show status))

-- Close position by deal ID
closePositionById :: BrokerConfig -> IGSession -> Text -> IGDealRequest -> IO (Result IGDealResponse)
closePositionById config session dealId closeReq = do
  compLogInfo dealsLogger ("Closing position by ID: " <> dealId)

  result <- try $ do
    request <- createJsonRequest config session "DELETE" ("/positions/" <> T.unpack dealId) "1" (JSON.toJSON closeReq)
    response <- httpLbs request =<< newManager tlsManagerSettings
    return response

  case result of
    Left (ex :: SomeException) -> do
      compLogError dealsLogger ("Failed to close position by ID: " <> T.pack (show ex))
      return $ Left $ brokerError ("Failed to close position by ID: " <> T.pack (show ex))

    Right response -> do
      let status = responseStatus response
          body = responseBody response

      if statusCode status == 200
        then case JSON.decode body :: Maybe IGDealResponse of
          Just dealResp -> do
            compLogInfo dealsLogger ("Position close by ID submitted: " <> dealResponseReference dealResp)
            return $ Right dealResp
          Nothing -> do
            compLogError dealsLogger "Failed to parse close by ID response"
            return $ Left $ brokerError "Failed to parse close by ID response"
        else do
          compLogError dealsLogger ("Close position by ID failed with status: " <> T.pack (show status))
          return $ Left $ brokerError ("Close position by ID failed with status: " <> T.pack (show status))

-- Working Orders Management Functions

-- Get all working orders
getWorkingOrders :: BrokerConfig -> IGSession -> IO (Result [IGWorkingOrder])
getWorkingOrders config session = do
  compLogInfo dealsLogger "Fetching all working orders"

  result <- try $ do
    request <- createVersionedRequest config session "/working-orders" "2"
    response <- httpLbs request =<< newManager tlsManagerSettings
    return response

  case result of
    Left (ex :: SomeException) -> do
      compLogError dealsLogger ("Failed to fetch working orders: " <> T.pack (show ex))
      return $ Left $ brokerError ("Failed to fetch working orders: " <> T.pack (show ex))

    Right response -> do
      let status = responseStatus response
          body = responseBody response

      if statusCode status == 200
        then case JSON.decode body :: Maybe IGWorkingOrderResponse of
          Just orderResp -> do
            compLogInfo dealsLogger ("Found " <> T.pack (show (length (workingOrdersData orderResp))) <> " working orders")
            return $ Right $ workingOrdersData orderResp
          Nothing -> do
            compLogError dealsLogger "Failed to parse working orders response"
            return $ Left $ brokerError "Failed to parse working orders response"
        else do
          compLogError dealsLogger ("Get working orders failed with status: " <> T.pack (show status))
          return $ Left $ brokerError ("Get working orders failed with status: " <> T.pack (show status))

-- Create working order
createWorkingOrder :: BrokerConfig -> IGSession -> IGWorkingOrderRequest -> IO (Result IGDealResponse)
createWorkingOrder config session orderReq = do
  compLogInfo dealsLogger ("Creating working order: " <> workingOrderReqEpic orderReq)

  result <- try $ do
    request <- createJsonRequest config session "POST" "/working-orders/otc" "2" (JSON.toJSON orderReq)
    response <- httpLbs request =<< newManager tlsManagerSettings
    return response

  case result of
    Left (ex :: SomeException) -> do
      compLogError dealsLogger ("Failed to create working order: " <> T.pack (show ex))
      return $ Left $ brokerError ("Failed to create working order: " <> T.pack (show ex))

    Right response -> do
      let status = responseStatus response
          body = responseBody response

      if statusCode status == 200
        then case JSON.decode body :: Maybe IGDealResponse of
          Just dealResp -> do
            compLogInfo dealsLogger ("Working order creation submitted: " <> dealResponseReference dealResp)
            return $ Right dealResp
          Nothing -> do
            compLogError dealsLogger "Failed to parse working order response"
            return $ Left $ brokerError "Failed to parse working order response"
        else do
          compLogError dealsLogger ("Create working order failed with status: " <> T.pack (show status))
          return $ Left $ brokerError ("Create working order failed with status: " <> T.pack (show status))

-- Amend working order
amendWorkingOrder :: BrokerConfig -> IGSession -> Text -> IGWorkingOrderRequest -> IO (Result IGDealResponse)
amendWorkingOrder config session dealId orderReq = do
  compLogInfo dealsLogger ("Amending working order: " <> dealId)

  result <- try $ do
    request <- createJsonRequest config session "PUT" ("/working-orders/otc/" <> T.unpack dealId) "2" (JSON.toJSON orderReq)
    response <- httpLbs request =<< newManager tlsManagerSettings
    return response

  case result of
    Left (ex :: SomeException) -> do
      compLogError dealsLogger ("Failed to amend working order: " <> T.pack (show ex))
      return $ Left $ brokerError ("Failed to amend working order: " <> T.pack (show ex))

    Right response -> do
      let status = responseStatus response
          body = responseBody response

      if statusCode status == 200
        then case JSON.decode body :: Maybe IGDealResponse of
          Just dealResp -> do
            compLogInfo dealsLogger ("Working order amendment submitted: " <> dealResponseReference dealResp)
            return $ Right dealResp
          Nothing -> do
            compLogError dealsLogger "Failed to parse working order amendment response"
            return $ Left $ brokerError "Failed to parse working order amendment response"
        else do
          compLogError dealsLogger ("Amend working order failed with status: " <> T.pack (show status))
          return $ Left $ brokerError ("Amend working order failed with status: " <> T.pack (show status))

-- Delete working order
deleteWorkingOrder :: BrokerConfig -> IGSession -> Text -> IO (Result IGDealResponse)
deleteWorkingOrder config session dealId = do
  compLogInfo dealsLogger ("Deleting working order: " <> dealId)

  result <- try $ do
    request <- createVersionedRequest config session ("/working-orders/otc/" <> T.unpack dealId) "1"
    let deleteRequest = request { method = "DELETE" }
    response <- httpLbs deleteRequest =<< newManager tlsManagerSettings
    return response

  case result of
    Left (ex :: SomeException) -> do
      compLogError dealsLogger ("Failed to delete working order: " <> T.pack (show ex))
      return $ Left $ brokerError ("Failed to delete working order: " <> T.pack (show ex))

    Right response -> do
      let status = responseStatus response
          body = responseBody response

      if statusCode status == 200
        then case JSON.decode body :: Maybe IGDealResponse of
          Just dealResp -> do
            compLogInfo dealsLogger ("Working order deletion submitted: " <> dealResponseReference dealResp)
            return $ Right dealResp
          Nothing -> do
            compLogError dealsLogger "Failed to parse working order deletion response"
            return $ Left $ brokerError "Failed to parse working order deletion response"
        else do
          compLogError dealsLogger ("Delete working order failed with status: " <> T.pack (show status))
          return $ Left $ brokerError ("Delete working order failed with status: " <> T.pack (show status))

-- Deal Confirmation Functions

-- Get deal confirmation
getDealConfirmation :: BrokerConfig -> IGSession -> Text -> IO (Result IGDealConfirmation)
getDealConfirmation config session dealReference = do
  compLogInfo dealsLogger ("Fetching deal confirmation: " <> dealReference)

  result <- try $ do
    request <- createVersionedRequest config session ("/confirms/" <> T.unpack dealReference) "1"
    response <- httpLbs request =<< newManager tlsManagerSettings
    return response

  case result of
    Left (ex :: SomeException) -> do
      compLogError dealsLogger ("Failed to fetch deal confirmation: " <> T.pack (show ex))
      return $ Left $ brokerError ("Failed to fetch deal confirmation: " <> T.pack (show ex))

    Right response -> do
      let status = responseStatus response
          body = responseBody response

      compLogDebug dealsLogger ("Deal confirmation response status: " <> T.pack (show status))
      compLogDebug dealsLogger ("Deal confirmation response body: " <> T.pack (show (LBS.take 1000 body)))

      if statusCode status == 200
        then case JSON.decode body :: Maybe IGDealConfirmation of
          Just confirmation -> do
            compLogInfo dealsLogger ("Retrieved deal confirmation: " <> dealReference)
            -- Always log the deal status and reason for troubleshooting - using INFO level
            case confirmationDealStatus confirmation of
              Just REJECTED -> do
                let reason = maybe "No reason provided" id (confirmationReason confirmation)
                compLogInfo dealsLogger ("Deal REJECTED - Reason: " <> reason)
              Just dealStatus -> compLogInfo dealsLogger ("Deal status: " <> T.pack (show dealStatus))
              Nothing -> compLogInfo dealsLogger "Deal status not provided in confirmation"

            -- Also log the reason field regardless of status
            case confirmationReason confirmation of
              Just reason -> compLogInfo dealsLogger ("Deal reason: " <> reason)
              Nothing -> compLogInfo dealsLogger "No reason provided in confirmation"

            return $ Right confirmation
          Nothing -> do
            compLogError dealsLogger "Failed to parse deal confirmation response"
            compLogDebug dealsLogger ("Raw response body: " <> T.pack (show (LBS.take 1000 body)))
            return $ Left $ brokerError "Failed to parse deal confirmation response"
        else do
          compLogError dealsLogger ("Get deal confirmation failed with status: " <> T.pack (show status))
          compLogDebug dealsLogger ("Response body: " <> T.pack (show (LBS.take 1000 body)))
          return $ Left $ brokerError ("Get deal confirmation failed with status: " <> T.pack (show status))
