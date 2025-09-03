{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}

module Adapter.IG.Polling
  ( -- Functions
    igStreamingLoop
  , pollIGMarketData
  , mockTickFetch
  , convertIGMarketToTick
  , instrumentToIGEpic
  , backoffDelay
  , secondsToMicros
  ) where

import Domain.Types
import Util.Config (BrokerConfig(..), ReconnectPolicy(..))
import Util.Error (Result, TempehError(..), BrokerErrorDetails(..), brokerError, getRecoveryStrategy, RecoveryStrategy(..), ErrorSeverity(..))
import Adapter.IG.Types
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Control.Exception (try, SomeException, displayException)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, getCurrentTime, diffUTCTime, NominalDiffTime)
import qualified Data.Map as Map
import Network.HTTP.Conduit
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import Util.Logger (logInfo, logError, logWarn, logDebug, ComponentName(..), runFileLoggerWithComponent)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Scientific (Scientific)

-- Adapter-scoped logging helpers
brokerLogInfo :: Text -> IO ()
brokerLogInfo msg = runFileLoggerWithComponent (ComponentName "BROKER") $ logInfo msg

brokerLogWarn :: Text -> IO ()
brokerLogWarn msg = runFileLoggerWithComponent (ComponentName "BROKER") $ logWarn msg

brokerLogError :: Text -> IO ()
brokerLogError msg = runFileLoggerWithComponent (ComponentName "BROKER") $ logError msg

-- Helper: append to a bounded buffer (drop oldest on overflow)
appendBounded :: Int -> [Tick] -> [Tick] -> [Tick]
appendBounded maxSize buf newTicks =
  let xs = buf ++ newTicks
      overflow = length xs - maxSize
  in if maxSize <= 0 then [] else if overflow > 0 then drop overflow xs else xs

-- Streaming loop (currently polls IG REST or generates demo ticks; structured for Lightstreamer)
igStreamingLoop :: BrokerConnection -> Instrument -> IO ()
igStreamingLoop conn instrument = do
  brokerLogInfo ("Starting market data streaming loop for " <> T.pack (show instrument))
  loop 0
  where
    loop attempt = do
      unsubscribed <- atomically $ do
        subs <- readTVar (bcSubscriptions conn)
        pure (Map.notMember instrument subs)
      if unsubscribed
        then brokerLogInfo ("Subscription removed; stopping streaming for " <> T.pack (show instrument))
        else do
          result <- do
            mSession <- atomically $ readTVar (bcIGSession conn)
            case mSession of
              Nothing -> mockTickFetch instrument
              Just session -> pollIGMarketData (bcConfig conn) session instrument
          case result of
            Left err -> do
              brokerLogError ("Market data streaming error: " <> T.pack (show err))
              handlePollingError err attempt
            Right ticks -> do
              now <- getCurrentTime
              atomically $ do
                writeTVar (bcLastHeartbeat conn) now
                writeTVar (bcStatus conn) (Connected now)
                subs <- readTVar (bcSubscriptions conn)
                case Map.lookup instrument subs of
                  Nothing -> pure ()
                  Just ss -> do
                    currentTicks <- readTVar (ssTickBuffer ss)
                    let capped = appendBounded (bcBufferSize conn) currentTicks ticks
                    writeTVar (ssTickBuffer ss) capped
                    -- metrics
                    let receivedNow = length ticks
                    modifyTVar' (ssTicksReceived ss) (+ receivedNow)
                    let lastTime = if null ticks then Nothing else Just (tTime (last ticks))
                    writeTVar (ssLastTickTime ss) lastTime
              let delayMicros = max 1 (1000000 `div` bcMaxTicksPerSecond conn)
              threadDelay delayMicros
              loop 0

    handlePollingError :: TempehError -> Int -> IO ()
    handlePollingError err attempt = do
      let recoveryAction = getRecoveryStrategy err
          rp = bcReconnectPolicy (bcConfig conn)
          nextAttempt = attempt + 1

      case recoveryAction of
        Just (Recoverable strategy) -> do
          if nextAttempt > rpMaxRetries rp
            then do
              brokerLogError "Max retries reached; marking connection as Failed."
              atomically $ writeTVar (bcStatus conn) (Failed "Max retries reached in streaming loop")
            else do
              atomically $ writeTVar (bcStatus conn) (Reconnecting nextAttempt)
              let delaySec = case strategy of
                    RetryWithBackoff _ baseDelay -> fromIntegral baseDelay * (2 ^ (nextAttempt - 1))
                    _ -> backoffDelay rp nextAttempt
                  delayMicros = secondsToMicros delaySec

              brokerLogWarn $ "Recoverable error. Retrying in " <> T.pack (show delaySec) <> "s (attempt " <> T.pack (show nextAttempt) <> ")"
              threadDelay delayMicros
              loop nextAttempt

        Just Fatal -> do
          brokerLogError "Fatal error encountered in streaming loop. Stopping."
          atomically $ writeTVar (bcStatus conn) (Failed "Fatal error in streaming loop")

        Nothing -> do
          brokerLogError "Unknown error type. Stopping."
          atomically $ writeTVar (bcStatus conn) (Failed "Unknown error in streaming loop")

-- Demo/mock tick fetch (simple heartbeat tick)
mockTickFetch :: Instrument -> IO (Result [Tick])
mockTickFetch instrument = do
  now <- getCurrentTime
  let mid = 1.0 :: Scientific
      spread = 0.0002 :: Scientific
      bid = Price (realToFrac (mid - spread/2))
      ask = Price (realToFrac (mid + spread/2))
      tick = Tick { tTime = now, tInstr = instrument, tBid = bid, tAsk = ask, tVolume = Nothing }
  pure (Right [tick])

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

          request' <- parseRequest $ "GET " <> marketUrl
          let request = request'
                { requestHeaders =
                    [ ("Accept", "application/json")
                    , ("Version", "3")
                    , ("X-IG-API-KEY", TE.encodeUtf8 $ maybe "" id (bcApiKey config))
                    , ("CST", TE.encodeUtf8 $ igCST session)
                    , ("X-SECURITY-TOKEN", TE.encodeUtf8 $ igXSecurityToken session)
                    ]
                }

          result <- (try :: IO (Response LBS.ByteString) -> IO (Either SomeException (Response LBS.ByteString))) $ httpLbs request =<< newManager tlsManagerSettings
          case result of
            Left (ex :: SomeException) -> return $ Left $ brokerError ("HTTP request failed: " <> T.pack (displayException ex))
            Right response -> do
              let body = responseBody response
              case eitherDecode body of
                Left parseErr -> return $ Left $ brokerError ("Failed to parse IG market data: " <> T.pack parseErr)
                Right igMarket -> do
                  now <- getCurrentTime
                  let tick = convertIGMarketToTick now instrument igMarket
                  return $ Right [tick]

-- Convert IG market data to our internal tick format
convertIGMarketToTick :: UTCTime -> Instrument -> IGMarket -> Tick
convertIGMarketToTick timestamp instrument igMarket = Tick
  { tTime = timestamp
  , tInstr = instrument
  , tBid = Price $ realToFrac $ maybe 0 id (marketBid igMarket)
  , tAsk = Price $ realToFrac $ maybe 0 id (marketAsk igMarket)
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

-- Helper functions
secondsToMicros :: NominalDiffTime -> Int
secondsToMicros s = floor (realToFrac s * 1_000_000 :: Double)

backoffDelay :: ReconnectPolicy -> Int -> NominalDiffTime
backoffDelay rp attempt = min (rpMaxDelay rp) (rpInitialDelay rp * realToFrac ((rpBackoffMultiplier rp) ** fromIntegral (max 0 (attempt - 1))))
