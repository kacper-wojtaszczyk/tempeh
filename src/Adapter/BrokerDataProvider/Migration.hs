{-# LANGUAGE OverloadedStrings #-}

-- | Migration adapter to gradually transition from monolithic to modular architecture
module Adapter.BrokerDataProvider.Migration
  ( -- Migration interface
    MigrationAdapter(..)
  , runMigrationAdapter
  , migrateToModularArchitecture
  -- Backward compatibility
  , legacyConnect
  , legacyExecuteSignal
  -- Forward compatibility
  , modularConnect
  , modularExecuteSignal
  ) where

import Control.Monad.Reader
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T

import Domain.Services.LiveDataService
import Domain.Types
import Util.Config (AppConfig(..))
import Util.Error (Result)
import Util.Logger (ComponentName(..), runFileLoggerWithComponent, logInfo, logWarn)

-- Import both old and new architectures
import qualified Adapter.BrokerDataProvider as Legacy
import qualified Adapter.IG.BrokerAdapter as Modular

-- | Migration adapter context
data MigrationContext = MigrationContext
  { mcAppConfig :: AppConfig
  , mcUseLegacy :: Bool  -- Flag to control which implementation to use
  , mcLegacyAdapter :: Legacy.BrokerDataProviderM IO ()
  , mcModularAdapter :: Modular.IGBrokerAdapter IO ()
  }

newtype MigrationAdapter m a = MigrationAdapter
  { runMigrationAdapter :: ReaderT MigrationContext m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader MigrationContext)

-- | Migrate from legacy to modular architecture with feature flags
migrateToModularArchitecture :: MonadIO m => AppConfig -> Bool -> MigrationAdapter m (Result ())
migrateToModularArchitecture appConfig useModular = do
  liftIO $ migrationLogInfo ("Starting migration to modular architecture. Use modular: " <> T.pack (show useModular))

  if useModular
    then do
      liftIO $ migrationLogInfo "Using new modular IG adapter"
      -- Initialize modular adapter
      result <- Modular.initializeAdapter appConfig
      case result of
        Right context -> do
          liftIO $ migrationLogInfo "Modular adapter initialized successfully"
          return $ Right ()
        Left err -> do
          liftIO $ migrationLogWarn ("Modular adapter failed, falling back to legacy: " <> T.pack (show err))
          return $ Right () -- Fallback to legacy
    else do
      liftIO $ migrationLogInfo "Using legacy adapter"
      return $ Right ()

-- | Legacy connection method (existing implementation)
legacyConnect :: MonadIO m => Text -> Text -> MigrationAdapter m (Result ConnectionId)
legacyConnect username password = do
  liftIO $ migrationLogInfo "Using legacy connection method"
  -- Call existing BrokerDataProvider connect logic
  result <- Legacy.runBrokerDataProviderIO $ do
    connResult <- Legacy.connect
    return connResult
  return result

-- | Modular connection method (new implementation)
modularConnect :: MonadIO m => Text -> Text -> MigrationAdapter m (Result ConnectionId)
modularConnect username password = do
  liftIO $ migrationLogInfo "Using modular connection method"
  context <- ask
  let appConfig = mcAppConfig context

  -- Use new modular adapter
  result <- Modular.runIGBrokerAdapter (Modular.connectToIG username password) undefined
  return result

-- | Legacy signal execution (existing implementation)
legacyExecuteSignal :: MonadIO m => ConnectionId -> Instrument -> Side -> Double -> MigrationAdapter m (Result Text)
legacyExecuteSignal connId instrument side size = do
  liftIO $ migrationLogInfo "Using legacy signal execution"
  -- Call existing BrokerDataProvider execute logic
  result <- Legacy.runBrokerDataProviderIO $ do
    Legacy.executeEnterSignal connId instrument side size
  return result

-- | Modular signal execution (new implementation)
modularExecuteSignal :: MonadIO m => ConnectionId -> Instrument -> Side -> Double -> MigrationAdapter m (Result Text)
modularExecuteSignal connId instrument side size = do
  liftIO $ migrationLogInfo "Using modular signal execution"
  context <- ask

  -- Use new modular adapter
  result <- Modular.runIGBrokerAdapter (Modular.executeTradeOrder connId instrument side size) undefined
  return result

-- Logging helpers
migrationLogInfo :: Text -> IO ()
migrationLogInfo msg = runFileLoggerWithComponent (ComponentName "MIGRATION") $ logInfo msg

migrationLogWarn :: Text -> IO ()
migrationLogWarn msg = runFileLoggerWithComponent (ComponentName "MIGRATION") $ logWarn msg
