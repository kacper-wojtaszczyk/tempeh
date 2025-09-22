{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Session management module - handles IG authentication and session lifecycle
module Adapter.IG.Session
  ( -- Session operations
    createSession
  , renewSession
  , closeSession
  , validateSession
  -- Session state
  , SessionManager(..)
  ) where

import Control.Monad.Reader
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.STM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, addUTCTime)

import Adapter.IG.Types (IGSession(..))
import Adapter.IG.Auth (loginToIG, logoutFromIG)
import Util.Config (BrokerConfig)
import Util.Error (Result, TempehError)
import Util.Logger (ComponentName(..), runFileLoggerWithComponent, logInfo, logWarn, logError)

-- | Session manager monad for handling IG session lifecycle
newtype SessionManager m a = SessionManager
  { runSessionManager :: ReaderT (TVar (Maybe IGSession)) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (TVar (Maybe IGSession)))

-- Session management operations
createSession :: MonadIO m => BrokerConfig -> Text -> Text -> SessionManager m (Result IGSession)
createSession config username password = do
  liftIO $ sessionLogInfo "Creating new IG session"
  result <- liftIO $ loginToIG config username password
  case result of
    Right session -> do
      sessionVar <- ask
      liftIO $ atomically $ writeTVar sessionVar (Just session)
      liftIO $ sessionLogInfo "Session created successfully"
      return $ Right session
    Left err -> do
      liftIO $ sessionLogError ("Failed to create session: " <> T.pack (show err))
      return $ Left err

renewSession :: MonadIO m => BrokerConfig -> Text -> Text -> SessionManager m (Result IGSession)
renewSession config username password = do
  liftIO $ sessionLogInfo "Renewing IG session"
  -- Close existing session first
  _ <- closeSession config
  -- Create new session
  createSession config username password

closeSession :: MonadIO m => BrokerConfig -> SessionManager m (Result ())
closeSession config = do
  sessionVar <- ask
  maybeSession <- liftIO $ readTVarIO sessionVar
  case maybeSession of
    Nothing -> return $ Right ()
    Just session -> do
      result <- liftIO $ logoutFromIG config session
      liftIO $ atomically $ writeTVar sessionVar Nothing
      liftIO $ sessionLogInfo "Session closed"
      return result

validateSession :: MonadIO m => SessionManager m Bool
validateSession = do
  sessionVar <- ask
  maybeSession <- liftIO $ readTVarIO sessionVar
  now <- liftIO getCurrentTime
  case maybeSession of
    Nothing -> return False
    Just session -> return $ igExpiresAt session > now

-- Logging helpers
sessionLogInfo :: Text -> IO ()
sessionLogInfo msg = runFileLoggerWithComponent (ComponentName "IG_SESSION") $ logInfo msg

sessionLogWarn :: Text -> IO ()
sessionLogWarn msg = runFileLoggerWithComponent (ComponentName "IG_SESSION") $ logWarn msg

sessionLogError :: Text -> IO ()
sessionLogError msg = runFileLoggerWithComponent (ComponentName "IG_SESSION") $ logError msg
