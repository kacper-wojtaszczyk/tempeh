{-# LANGUAGE OverloadedStrings #-}
module Util.Logger where

import Util.Config (LogLevel(..), LogConfig(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.IO (hPutStrLn, stderr, Handle, stdout)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Logger monad for dependency injection
class Monad m => MonadLogger m where
  logDebug :: T.Text -> m ()
  logInfo :: T.Text -> m ()
  logWarn :: T.Text -> m ()
  logError :: T.Text -> m ()

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

-- Helper functions for cleaner logging
logWithLevel :: MonadIO m => Logger -> LogConfig -> T.Text -> T.Text -> m ()
logWithLevel logger config levelStr msg = liftIO $ do
  let level = case levelStr of
        "Debug" -> Debug
        "Info"  -> Info
        "Warn"  -> Warn
        "Error" -> Error
        _       -> Info  -- Default fallback
  runLogger logger config msg level
