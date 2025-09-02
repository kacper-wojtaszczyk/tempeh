{-# LANGUAGE DeriveGeneric #-}
module Util.Config where

import GHC.Generics (Generic)

-- Application configuration
data AppConfig = AppConfig
  { acDataDirectory :: FilePath
  , acLogging :: LogConfig
  } deriving (Show, Generic)

-- Logging configuration
data LogConfig = LogConfig
  { lcLevel :: LogLevel
  , lcOutputFile :: Maybe FilePath
  } deriving (Show, Generic)

data LogLevel = Debug | Info | Warn | Error
  deriving (Show, Eq, Ord, Generic)

-- Smart constructor for default configuration
defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig
  { acDataDirectory = "data/backtesting"
  , acLogging = defaultLogConfig
  }

defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig
  { lcLevel = Info
  , lcOutputFile = Nothing
  }
