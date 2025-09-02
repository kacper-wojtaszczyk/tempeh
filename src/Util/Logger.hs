{-# LANGUAGE OverloadedStrings #-}
module Util.Logger
  ( logDebug
  , logInfo
  , logWarn
  , logError
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- | Log a message at DEBUG level.
logDebug :: MonadIO m => T.Text -> m ()
logDebug msg = liftIO $ do
  timestamp <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
  TIO.putStrLn $ T.pack timeStr <> " [DEBUG] " <> msg

-- | Log a message at INFO level.
logInfo :: MonadIO m => T.Text -> m ()
logInfo msg = liftIO $ do
  timestamp <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
  TIO.putStrLn $ T.pack timeStr <> " [INFO]  " <> msg

-- | Log a message at WARN level.
logWarn :: MonadIO m => T.Text -> m ()
logWarn msg = liftIO $ do
  timestamp <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
  TIO.putStrLn $ T.pack timeStr <> " [WARN]  " <> msg

-- | Log a message at ERROR level.
logError :: MonadIO m => T.Text -> m ()
logError msg = liftIO $ do
  timestamp <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
  TIO.putStrLn $ T.pack timeStr <> " [ERROR] " <> msg
