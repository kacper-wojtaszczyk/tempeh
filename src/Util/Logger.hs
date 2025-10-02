{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Util.Logger
  ( -- Core logging interface
    MonadLogger(..)
  , LogLevel(..)
  , LogEvent(..)
  , CorrelationId(..)
  , ComponentName(..)
  , LogContext(..)
  , emptyLogContext

  -- Structured logging
  , logWithContext
  , withCorrelationId
  , withComponent

  -- Convenience functions
  , logInfo
  , logError
  , logWarn
  , logDebug

  -- Component logger factory (NEW)
  , ComponentLogger(..)
  , makeComponentLogger

  -- Component logging functions (ADDED)
  , compLogInfo
  , compLogWarn
  , compLogError
  , compLogDebug

  -- Implementation
  , runConsoleLogger
  , runFileLogger
  , runStructuredLogger
  , generateLogFileName
  , runFileLoggerWithComponent
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, asks, local)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), object, Value, encode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time (UTCTime, getCurrentTime, formatTime, defaultTimeLocale)
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (Handle, openFile, hClose, IOMode(..), hFlush)
import Control.Exception (bracket) -- Added missing import
import qualified Data.ByteString.Lazy as LBS

-- Core types
data LogLevel = Debug | Info | Warn | Error
  deriving (Show, Eq, Ord, Generic)

instance ToJSON LogLevel
instance FromJSON LogLevel

newtype CorrelationId = CorrelationId { unCorrelationId :: Text }
  deriving (Show, Eq, Generic)

instance ToJSON CorrelationId
instance FromJSON CorrelationId

newtype ComponentName = ComponentName { unComponentName :: Text }
  deriving (Show, Eq, Generic)

instance ToJSON ComponentName
instance FromJSON ComponentName

-- Structured log event
data LogEvent = LogEvent
  { leLevel :: LogLevel
  , leTimestamp :: UTCTime
  , leCorrelationId :: Maybe CorrelationId
  , leComponent :: Maybe ComponentName
  , leMessage :: Text
  , leMetadata :: Map.Map Text Value
  } deriving (Show, Generic)

instance ToJSON LogEvent where
  toJSON (LogEvent level ts corrId comp msg meta) = object
    [ "level" .= level
    , "timestamp" .= ts
    , "correlationId" .= corrId
    , "component" .= comp
    , "message" .= msg
    , "metadata" .= meta
    ]

-- Logging context for Reader monad
data LogContext = LogContext
  { lcCorrelationId :: Maybe CorrelationId
  , lcComponent :: Maybe ComponentName
  , lcLogFile :: Maybe FilePath  -- New field for specific log file
  } deriving (Show)

emptyLogContext :: LogContext
emptyLogContext = LogContext Nothing Nothing Nothing

-- Logging monad class - fits your existing pattern
class Monad m => MonadLogger m where
  logAtLevel :: LogLevel -> Text -> Map.Map Text Value -> m ()

-- Structured logging with context
logWithContext :: MonadLogger m => LogLevel -> Text -> Map.Map Text Value -> m ()
logWithContext = logAtLevel

-- Context manipulation
withCorrelationId :: Monad m => CorrelationId -> ReaderT LogContext m a -> ReaderT LogContext m a
withCorrelationId corrId = local (\ctx -> ctx { lcCorrelationId = Just corrId })

withComponent :: Monad m => ComponentName -> ReaderT LogContext m a -> ReaderT LogContext m a
withComponent comp = local (\ctx -> ctx { lcComponent = Just comp })

-- Convenience functions
logInfo :: MonadLogger m => Text -> m ()
logInfo msg = logAtLevel Info msg Map.empty

logError :: MonadLogger m => Text -> m ()
logError msg = logAtLevel Error msg Map.empty

logWarn :: MonadLogger m => Text -> m ()
logWarn msg = logAtLevel Warn msg Map.empty

logDebug :: MonadLogger m => Text -> m ()
logDebug msg = logAtLevel Debug msg Map.empty

-- Implementation for ReaderT LogContext IO - enhanced to support file logging
instance MonadIO m => MonadLogger (ReaderT LogContext m) where
  logAtLevel level msg metadata = do
    ctx <- asks id
    ts <- liftIO getCurrentTime
    let event = LogEvent
          { leLevel = level
          , leTimestamp = ts
          , leCorrelationId = lcCorrelationId ctx
          , leComponent = lcComponent ctx
          , leMessage = msg
          , leMetadata = metadata
          }
    case lcLogFile ctx of
      Just filePath -> liftIO $ writeLogEventToFile filePath event
      Nothing -> liftIO $ formatAndPrintLogEvent event

-- Console formatter
formatAndPrintLogEvent :: LogEvent -> IO ()
formatAndPrintLogEvent event = do
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (leTimestamp event)
      level = show (leLevel event)
      corrId = maybe "" ((" [" <>) . (<> "]") . unCorrelationId) (leCorrelationId event)
      comp = maybe "" ((" (" <>) . (<> ")") . unComponentName) (leComponent event)
      formatted = T.pack timestamp <> " [" <> T.pack level <> "]" <> corrId <> comp <> "  " <> leMessage event
  TIO.putStrLn formatted

-- New: Write log event to file with proper error handling
writeLogEventToFile :: FilePath -> LogEvent -> IO ()
writeLogEventToFile filePath event = do
  -- Ensure log directory exists
  createDirectoryIfMissing True (takeDirectory filePath)

  -- Format log entry for file output
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (leTimestamp event)
      level = show (leLevel event)
      corrId = maybe "" ((" [" <>) . (<> "]") . unCorrelationId) (leCorrelationId event)
      comp = maybe "" ((" (" <>) . (<> ")") . unComponentName) (leComponent event)
      formatted = T.pack timestamp <> " [" <> T.pack level <> "]" <> corrId <> comp <> "  " <> leMessage event

  -- Append to log file with proper error handling
  bracket (openFile filePath AppendMode) hClose $ \handle -> do
    TIO.hPutStrLn handle formatted
    hFlush handle

-- New: Generate standardized log file names (daily grouping)
generateLogFileName :: ComponentName -> IO FilePath
generateLogFileName (ComponentName comp) = do
  now <- getCurrentTime
  let dateStamp = formatTime defaultTimeLocale "%Y-%m-%d" now
      fileName = T.unpack comp <> "-" <> dateStamp <> ".log"
  return $ "log" </> fileName

-- Helper function to extract directory from file path
takeDirectory :: FilePath -> FilePath
takeDirectory path = case reverse (splitOn '/' path) of
  [] -> "."
  [_] -> "."
  (_:dirs) -> intercalate "/" (reverse dirs)
  where
    splitOn :: Char -> String -> [String]
    splitOn _ [] = []
    splitOn delimiter str =
      let (prefix, suffix) = break (== delimiter) str
      in prefix : case suffix of
                   [] -> []
                   (_:rest) -> splitOn delimiter rest

    intercalate :: String -> [String] -> String
    intercalate _ [] = ""
    intercalate _ [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- Enhanced runner functions
runConsoleLogger :: ReaderT LogContext IO a -> IO a
runConsoleLogger action = runReaderT action emptyLogContext

-- Fully implemented file logger
runFileLogger :: FilePath -> ReaderT LogContext IO a -> IO a
runFileLogger filePath action = do
  let logContext = emptyLogContext { lcLogFile = Just filePath }
  runReaderT action logContext

-- Structured JSON logger (writes JSON to file)
runStructuredLogger :: FilePath -> ReaderT LogContext IO a -> IO a
runStructuredLogger filePath action = do
  let logContext = emptyLogContext { lcLogFile = Just filePath }
  runReaderT action logContext

-- New: Run with auto-generated log file name based on component
runFileLoggerWithComponent :: ComponentName -> ReaderT LogContext IO a -> IO a
runFileLoggerWithComponent comp action = do
  filePath <- generateLogFileName comp
  runFileLogger filePath action

-- Component logger record for eliminating duplication (NEW)
data ComponentLogger = ComponentLogger
  { clInfo  :: Text -> IO ()
  , clWarn  :: Text -> IO ()
  , clError :: Text -> IO ()
  , clDebug :: Text -> IO ()
  }

-- Factory function to create component-specific loggers (NEW)
makeComponentLogger :: Text -> ComponentLogger
makeComponentLogger componentName =
  let comp = ComponentName componentName
      logWithComp logFn msg = runFileLoggerWithComponent comp $ logFn msg
  in ComponentLogger
    { clInfo = logWithComp logInfo
    , clWarn = logWithComp logWarn
    , clError = logWithComp logError
    , clDebug = logWithComp logDebug
    }

-- Standalone component logging functions for backward compatibility
compLogInfo :: ComponentLogger -> Text -> IO ()
compLogInfo (ComponentLogger info _ _ _) = info

compLogWarn :: ComponentLogger -> Text -> IO ()
compLogWarn (ComponentLogger _ warn _ _) = warn

compLogError :: ComponentLogger -> Text -> IO ()
compLogError (ComponentLogger _ _ err _) = err

compLogDebug :: ComponentLogger -> Text -> IO ()
compLogDebug (ComponentLogger _ _ _ debug) = debug
