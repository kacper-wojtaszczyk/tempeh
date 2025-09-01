{-# LANGUAGE OverloadedStrings #-}
module Util.Logger
  ( LogLevel(..)
  , Logger(..)
  , mkLogger
  , logWithLevel
  , logDebug
  , logInfo
  , logWarn
  , logError
  , defaultLogger
  ) where

import Util.Config (LogLevel(..), LogConfig(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.IO (hPutStrLn, stderr, Handle, stdout)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Simple IO-based logger implementation
newtype Logger = Logger
  { runLogger :: LogConfig -> T.Text -> LogLevel -> IO ()
  }

-- Create a logger instance
mkLogger :: LogConfig -> Logger
mkLogger config = Logger $ \_ msg level -> do
  when (level >= lcLevel config) $ do
    timestamp <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
        levelStr = case level of
          Debug -> "[DEBUG]"
          Info  -> "[INFO] "
          Warn  -> "[WARN] "
          Error -> "[ERROR]"
        logLine = T.pack timeStr <> " " <> levelStr <> " " <> msg

    case lcOutputFile config of
      Nothing -> TIO.putStrLn logLine
      Just file -> TIO.appendFile file (logLine <> "\n")
  where
    when True action = action
    when False _ = pure ()

-- | Log a message with a specific log level.
logWithLevel :: MonadIO m => LogLevel -> T.Text -> m ()
logWithLevel level msg = liftIO $ do
  timestamp <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
      levelStr = case level of
        Debug -> "[DEBUG]"
        Info  -> "[INFO] "
        Warn  -> "[WARN] "
        Error -> "[ERROR]"
      logLine = T.pack timeStr <> " " <> levelStr <> " " <> msg
  TIO.putStrLn logLine

-- Convenient logging functions for direct use
logDebug :: MonadIO m => T.Text -> m ()
logDebug msg = liftIO $ do
  timestamp <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
  TIO.putStrLn $ T.pack timeStr <> " [DEBUG] " <> msg

logInfo :: MonadIO m => T.Text -> m ()
logInfo msg = liftIO $ do
  timestamp <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
  TIO.putStrLn $ T.pack timeStr <> " [INFO]  " <> msg

logWarn :: MonadIO m => T.Text -> m ()
logWarn msg = liftIO $ do
  timestamp <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
  TIO.putStrLn $ T.pack timeStr <> " [WARN]  " <> msg

logError :: MonadIO m => T.Text -> m ()
logError msg = liftIO $ do
  timestamp <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
  TIO.putStrLn $ T.pack timeStr <> " [ERROR] " <> msg

-- Default logger configuration
defaultLogger :: Logger
defaultLogger = mkLogger defaultLogConfig
  where
    defaultLogConfig = LogConfig
      { lcLevel = Info
      , lcOutputFile = Nothing
      }
