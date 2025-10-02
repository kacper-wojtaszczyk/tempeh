{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Util.Logger
  ( -- Core logging interface
    MonadLogger(..)
  , LogLevel(..)
  , LogEntry(..) -- JSON log entry format (now the only format)
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

  -- Component logger factory
  , ComponentLogger(..)
  , makeComponentLogger

  -- Component logging functions
  , compLogInfo
  , compLogWarn
  , compLogError
  , compLogDebug

  -- Implementation (JSON only)
  , runConsoleLogger
  , runFileLogger
  , generateLogFileName
  , runFileLoggerWithComponent
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, asks, local)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), object, Value, encode)
import qualified Data.Aeson as A
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
import Control.Exception (bracket)
import qualified Data.ByteString.Lazy as LBS

-- Core types
data LogLevel = Debug | Info | Warn | Error
  deriving (Show, Eq, Ord, Generic)

instance ToJSON LogLevel where
  toJSON Debug = "DEBUG"
  toJSON Info = "INFO"
  toJSON Warn = "WARN"
  toJSON Error = "ERROR"

instance FromJSON LogLevel where
  parseJSON = A.withText "LogLevel" $ \t -> case t of
    "DEBUG" -> return Debug
    "INFO" -> return Info
    "WARN" -> return Warn
    "ERROR" -> return Error
    _ -> fail $ "Invalid LogLevel: " ++ T.unpack t

newtype CorrelationId = CorrelationId { unCorrelationId :: Text }
  deriving (Show, Eq, Generic)

instance ToJSON CorrelationId
instance FromJSON CorrelationId

newtype ComponentName = ComponentName { unComponentName :: Text }
  deriving (Show, Eq, Generic)

instance ToJSON ComponentName
instance FromJSON ComponentName

-- JSON log entry format (now the ONLY format)
data LogEntry = LogEntry
  { logEntryTimestamp :: String     -- ISO 8601 format
  , logEntryLevel :: LogLevel       -- DEBUG|INFO|WARN|ERROR
  , logEntryComponent :: Text       -- Component name as text
  , logEntryMessage :: Text         -- Human readable message
  } deriving (Show, Generic)

instance ToJSON LogEntry where
  toJSON (LogEntry ts level comp msg) = object
    [ "ts" .= ts
    , "level" .= level
    , "component" .= comp
    , "msg" .= msg
    ]

instance FromJSON LogEntry where
  parseJSON = A.withObject "LogEntry" $ \o -> LogEntry
    <$> o A..: "ts"
    <*> o A..: "level"
    <*> o A..: "component"
    <*> o A..: "msg"

-- Simplified logging context (no dual-mode flags needed)
data LogContext = LogContext
  { lcCorrelationId :: Maybe CorrelationId
  , lcComponent :: Maybe ComponentName
  , lcLogFile :: Maybe FilePath
  } deriving (Show)

emptyLogContext :: LogContext
emptyLogContext = LogContext Nothing Nothing Nothing

-- Logging monad class
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

-- Implementation for ReaderT LogContext IO - JSON output only
instance MonadIO m => MonadLogger (ReaderT LogContext m) where
  logAtLevel level msg metadata = do
    ctx <- asks id
    ts <- liftIO getCurrentTime
    let jsonEntry = createJsonLogEntry level msg ctx ts
    case lcLogFile ctx of
      Just filePath -> liftIO $ writeJsonLogToFile filePath jsonEntry
      Nothing -> liftIO $ printJsonLog jsonEntry

-- Create JSON log entry from context and event data
createJsonLogEntry :: LogLevel -> Text -> LogContext -> UTCTime -> LogEntry
createJsonLogEntry level msg ctx ts = LogEntry
  { logEntryTimestamp = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.%qZ" ts
  , logEntryLevel = level
  , logEntryComponent = maybe "UNKNOWN" unComponentName (lcComponent ctx)
  , logEntryMessage = msg
  }

-- Print JSON log to console
printJsonLog :: LogEntry -> IO ()
printJsonLog entry = TIO.putStrLn $ TL.toStrict $ TLE.decodeUtf8 $ encode entry

-- Write JSON log to file
writeJsonLogToFile :: FilePath -> LogEntry -> IO ()
writeJsonLogToFile filePath entry = do
  -- Ensure log directory exists
  createDirectoryIfMissing True (takeDirectory filePath)

  -- Write JSON entry to file
  let jsonOutput = TL.toStrict $ TLE.decodeUtf8 $ encode entry
  bracket (openFile filePath AppendMode) hClose $ \handle -> do
    TIO.hPutStrLn handle jsonOutput
    hFlush handle

-- Generate standardized log file names (daily grouping)
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

-- Simplified runner functions (JSON only)
runConsoleLogger :: ReaderT LogContext IO a -> IO a
runConsoleLogger action = runReaderT action emptyLogContext

runFileLogger :: FilePath -> ReaderT LogContext IO a -> IO a
runFileLogger filePath action = do
  let logContext = emptyLogContext { lcLogFile = Just filePath }
  runReaderT action logContext

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
