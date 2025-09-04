{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Util.Config where

import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON, defaultOptions, (.=), object, (.:), (.:?), withObject)
import Data.Aeson.Types (Parser)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Data.Time (NominalDiffTime)
import System.Environment (lookupEnv)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

-- Main application configuration
data AppConfig = AppConfig
  { acDataDirectory :: FilePath
  , acLogging :: LogConfig
  , acBroker :: BrokerConfig
  , acLiveTrading :: LiveTradingConfig
  } deriving (Show, Generic)

instance ToJSON AppConfig where
  toJSON = genericToJSON defaultOptions
instance FromJSON AppConfig where
  parseJSON = genericParseJSON defaultOptions

-- Logging configuration
data LogConfig = LogConfig
  { lcLevel :: LogLevel
  , lcOutputFile :: Maybe FilePath
  , lcEnableConsole :: Bool
  , lcRotationSize :: Maybe Int  -- MB
  } deriving (Show, Generic)

instance ToJSON LogConfig where
  toJSON = genericToJSON defaultOptions
instance FromJSON LogConfig where
  parseJSON = genericParseJSON defaultOptions

data LogLevel = Debug | Info | Warn | Error
  deriving (Show, Eq, Ord, Generic)

instance ToJSON LogLevel where
  toJSON = genericToJSON defaultOptions
instance FromJSON LogLevel where
  parseJSON = genericParseJSON defaultOptions

-- Broker configuration
data BrokerConfig = BrokerConfig
  { bcBrokerType :: BrokerType
  , bcEnvironment :: BrokerEnvironment
  , bcBaseUrl :: Maybe Text
  , bcApiKey :: Maybe Text        -- Empty in global config, filled in local
  , bcUsername :: Maybe Text      -- Empty in global config, filled in local
  , bcPassword :: Maybe Text      -- Empty in global config, filled in local
  , bcAccountId :: Maybe Text     -- Empty in global config, filled in local
  , bcConnectTimeout :: Int       -- seconds
  , bcReadTimeout :: Int          -- seconds
  , bcReconnectPolicy :: ReconnectPolicy
  } deriving (Show, Generic)

instance ToJSON BrokerConfig where
  toJSON = genericToJSON defaultOptions
instance FromJSON BrokerConfig where
  parseJSON = genericParseJSON defaultOptions

data BrokerType =
    IG
  | OANDA
  | InteractiveBrokers
  | Alpaca
  | Demo  -- For testing
  deriving (Show, Eq, Generic)

instance ToJSON BrokerType where
  toJSON = genericToJSON defaultOptions
instance FromJSON BrokerType where
  parseJSON = genericParseJSON defaultOptions

data BrokerEnvironment =
    DemoEnv
  | SandboxEnv
  | LiveEnv
  deriving (Show, Eq, Generic)

instance ToJSON BrokerEnvironment where
  toJSON = genericToJSON defaultOptions
instance FromJSON BrokerEnvironment where
  parseJSON = genericParseJSON defaultOptions

data ReconnectPolicy = ReconnectPolicy
  { rpMaxRetries :: Int
  , rpInitialDelay :: NominalDiffTime
  , rpMaxDelay :: NominalDiffTime
  , rpBackoffMultiplier :: Double
  } deriving (Show, Generic)

instance ToJSON ReconnectPolicy where
  toJSON (ReconnectPolicy maxRetries initialDelay maxDelay multiplier) = object
    [ "maxRetries" .= maxRetries
    , "initialDelaySeconds" .= (fromRational $ toRational initialDelay :: Double)
    , "maxDelaySeconds" .= (fromRational $ toRational maxDelay :: Double)
    , "backoffMultiplier" .= multiplier
    ]

instance FromJSON ReconnectPolicy where
  parseJSON = withObject "ReconnectPolicy" $ \v -> ReconnectPolicy
    <$> v .: "maxRetries"
    <*> (fromRational . toRational <$> (v .: "initialDelaySeconds" :: Parser Double))
    <*> (fromRational . toRational <$> (v .: "maxDelaySeconds" :: Parser Double))
    <*> v .: "backoffMultiplier"

-- Live trading configuration
data LiveTradingConfig = LiveTradingConfig
  { ltcTickBufferSize :: Int
  , ltcMaxTicksPerSecond :: Double  -- Changed from Int to Double for fractional rates
  , ltcHeartbeatInterval :: NominalDiffTime
  , ltcDataQualityThreshold :: Double  -- 0.0-1.0
  , ltcEnablePaperTrading :: Bool
  } deriving (Show, Generic)

instance ToJSON LiveTradingConfig where
  toJSON (LiveTradingConfig bufferSize maxTicks heartbeat threshold paperTrading) = object
    [ "tickBufferSize" .= bufferSize
    , "maxTicksPerSecond" .= maxTicks
    , "heartbeatIntervalSeconds" .= (fromRational $ toRational heartbeat :: Double)
    , "dataQualityThreshold" .= threshold
    , "enablePaperTrading" .= paperTrading
    ]

instance FromJSON LiveTradingConfig where
  parseJSON = withObject "LiveTradingConfig" $ \v -> LiveTradingConfig
    <$> v .: "tickBufferSize"
    <*> v .: "maxTicksPerSecond"  -- Now accepts Double values
    <*> (fromRational . toRational <$> (v .: "heartbeatIntervalSeconds" :: Parser Double))
    <*> v .: "dataQualityThreshold"
    <*> v .: "enablePaperTrading"

-- Global test-config override used by tests to avoid env vars
{-# NOINLINE testConfigOverride #-}
testConfigOverride :: IORef (Maybe AppConfig)
testConfigOverride = unsafePerformIO (newIORef Nothing)

-- Set an in-memory AppConfig override (used by tests)
setTestConfig :: AppConfig -> IO ()
setTestConfig cfg = writeIORef testConfigOverride (Just cfg)

-- Load AppConfig from a specific file and set as override (used by tests)
setTestConfigFromFile :: FilePath -> IO (Either Text ())
setTestConfigFromFile path = do
  eCfg <- loadConfigFromFile path
  case eCfg of
    Left err -> pure (Left err)
    Right cfg -> do
      setTestConfig cfg
      pure (Right ())

-- Clear any test-config override
clearTestConfigOverride :: IO ()
clearTestConfigOverride = writeIORef testConfigOverride Nothing

-- Configuration loading and merging
loadAppConfig :: MonadIO m => m (Either Text AppConfig)
loadAppConfig = liftIO $ do
  -- If a test override is set, use it
  override <- readIORef testConfigOverride
  case override of
    Just cfg -> pure (Right cfg)
    Nothing -> do
      globalConfigResult <- loadGlobalConfig
      case globalConfigResult of
        Left err -> pure $ Left err
        Right globalConfig -> do
          localConfigResult <- loadLocalConfig
          pure $ case localConfigResult of
            Left _ -> Right globalConfig  -- Local config is optional
            Right localConfig -> Right $ mergeConfigs globalConfig localConfig

-- Load global configuration (public, versioned)
loadGlobalConfig :: IO (Either Text AppConfig)
loadGlobalConfig = loadConfigFromFile "config/global.json"

-- Load local configuration (private, gitignored)
loadLocalConfig :: IO (Either Text AppConfig)
loadLocalConfig = loadConfigFromFile "config/local.json"

-- Load configuration from specific file
loadConfigFromFile :: FilePath -> IO (Either Text AppConfig)
loadConfigFromFile path = do
  exists <- doesFileExist path
  if not exists
    then pure $ Left $ "Configuration file not found: " <> T.pack path
    else do
      content <- LBS.readFile path
      case JSON.eitherDecode content of
        Left err -> pure $ Left $ "Failed to parse config file " <> T.pack path <> ": " <> T.pack err
        Right config -> pure $ Right config

-- Merge local config over global config (local overrides global)
mergeConfigs :: AppConfig -> AppConfig -> AppConfig
mergeConfigs global local = AppConfig
  { acDataDirectory = acDataDirectory local  -- Local can override data directory
  , acLogging = mergeLogConfig (acLogging global) (acLogging local)
  , acBroker = mergeBrokerConfig (acBroker global) (acBroker local)
  , acLiveTrading = acLiveTrading local  -- Local overrides live trading settings
  }

mergeLogConfig :: LogConfig -> LogConfig -> LogConfig
mergeLogConfig global local = LogConfig
  { lcLevel = lcLevel local
  , lcOutputFile = lcOutputFile local
  , lcEnableConsole = lcEnableConsole local
  , lcRotationSize = lcRotationSize local
  }

mergeBrokerConfig :: BrokerConfig -> BrokerConfig -> BrokerConfig
mergeBrokerConfig global local = BrokerConfig
  { bcBrokerType = bcBrokerType local
  , bcEnvironment = bcEnvironment local
  , bcBaseUrl = bcBaseUrl local
  , bcApiKey = bcApiKey local          -- Credentials only come from local
  , bcUsername = bcUsername local      -- Credentials only come from local
  , bcPassword = bcPassword local      -- Credentials only come from local
  , bcAccountId = bcAccountId local    -- Account details only come from local
  , bcConnectTimeout = bcConnectTimeout local
  , bcReadTimeout = bcReadTimeout local
  , bcReconnectPolicy = bcReconnectPolicy local
  }

-- Smart constructors for default configurations
defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig
  { acDataDirectory = "data/backtesting"
  , acLogging = defaultLogConfig
  , acBroker = defaultBrokerConfig
  , acLiveTrading = defaultLiveTradingConfig
  }

defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig
  { lcLevel = Info
  , lcOutputFile = Nothing
  , lcEnableConsole = True
  , lcRotationSize = Just 100  -- 100MB
  }

defaultBrokerConfig :: BrokerConfig
defaultBrokerConfig = BrokerConfig
  { bcBrokerType = Demo
  , bcEnvironment = DemoEnv
  , bcBaseUrl = Nothing
  , bcApiKey = Nothing
  , bcUsername = Nothing
  , bcPassword = Nothing
  , bcAccountId = Nothing
  , bcConnectTimeout = 30
  , bcReadTimeout = 60
  , bcReconnectPolicy = defaultReconnectPolicy
  }

-- Force broker settings to Demo (used in tests/CI to avoid real HTTP calls)
forceDemoBroker :: AppConfig -> AppConfig
forceDemoBroker cfg = cfg
  { acBroker = (acBroker cfg)
      { bcBrokerType = Demo
      , bcEnvironment = DemoEnv
      , bcBaseUrl = Nothing
      , bcApiKey = Nothing
      , bcUsername = Nothing
      , bcPassword = Nothing
      , bcAccountId = Nothing
      }
  }

defaultReconnectPolicy :: ReconnectPolicy
defaultReconnectPolicy = ReconnectPolicy
  { rpMaxRetries = 5
  , rpInitialDelay = 1.0  -- 1 second
  , rpMaxDelay = 30.0     -- 30 seconds
  , rpBackoffMultiplier = 2.0
  }

defaultLiveTradingConfig :: LiveTradingConfig
defaultLiveTradingConfig = LiveTradingConfig
  { ltcTickBufferSize = 10000
  , ltcMaxTicksPerSecond = 1000
  , ltcHeartbeatInterval = 30.0  -- 30 seconds
  , ltcDataQualityThreshold = 0.95
  , ltcEnablePaperTrading = True
  }
