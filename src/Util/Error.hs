{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Util.Error
  ( -- Enhanced error types
    TempehError(..)
  , DataErrorDetails(..)
  , StrategyErrorDetails(..)
  , RiskErrorDetails(..)
  , ConfigErrorDetails(..)
  , SystemErrorDetails(..)
  , BrokerErrorDetails(..)

  -- Error context and recovery
  , ErrorSeverity(..)
  , RecoveryStrategy(..)
  , ErrorContext(..)

  -- Result type with enhanced context
  , Result
  , ResultWithContext(..)

  -- Legacy support
  , AppError(..)

  -- Convenience functions
  , withErrorContext
  , addErrorContext
  , isRecoverable
  , getRecoveryStrategy

  -- Error creation helpers
  , dataError
  , strategyError
  , riskError
  , configError
  , systemError
  , brokerError
  , fileError
  , unsupportedOperationError
  ) where

import Control.Exception (Exception)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), object)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Util.Logger (CorrelationId, ComponentName)

-- Enhanced error hierarchy
data TempehError =
    DataError DataErrorDetails
  | StrategyError StrategyErrorDetails
  | RiskError RiskErrorDetails
  | ConfigurationError ConfigErrorDetails
  | SystemError SystemErrorDetails
  | BrokerError BrokerErrorDetails
  deriving (Show, Eq, Generic)

instance Exception TempehError
instance ToJSON TempehError
instance FromJSON TempehError

-- Detailed error types
data DataErrorDetails = DataErrorDetails
  { dedMessage :: Text
  , dedDataSource :: Maybe Text
  , dedInstrument :: Maybe Text
  , dedRecordCount :: Maybe Int
  , dedQualityScore :: Maybe Double
  } deriving (Show, Eq, Generic)

instance ToJSON DataErrorDetails
instance FromJSON DataErrorDetails

data StrategyErrorDetails = StrategyErrorDetails
  { sedMessage :: Text
  , sedStrategyName :: Maybe Text
  , sedParameters :: Maybe Text
  , sedSignalCount :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON StrategyErrorDetails
instance FromJSON StrategyErrorDetails

data RiskErrorDetails = RiskErrorDetails
  { redMessage :: Text
  , redCurrentDrawdown :: Maybe Double
  , redMaxDrawdown :: Maybe Double
  , redPositionSize :: Maybe Double
  , redRiskLevel :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON RiskErrorDetails
instance FromJSON RiskErrorDetails

data ConfigErrorDetails = ConfigErrorDetails
  { cedMessage :: Text
  , cedConfigFile :: Maybe Text
  , cedField :: Maybe Text
  , cedEnvironment :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON ConfigErrorDetails
instance FromJSON ConfigErrorDetails

data SystemErrorDetails = SystemErrorDetails
  { sydMessage :: Text
  , sydComponent :: Maybe Text
  , sydSystemError :: Maybe Text
  , sydMemoryUsage :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON SystemErrorDetails
instance FromJSON SystemErrorDetails

data BrokerErrorDetails = BrokerErrorDetails
  { bedMessage :: Text
  , bedBrokerType :: Maybe Text
  , bedConnectionId :: Maybe Text
  , bedHttpStatus :: Maybe Int
  , bedRateLimit :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON BrokerErrorDetails
instance FromJSON BrokerErrorDetails

-- Error severity and recovery
data ErrorSeverity = Recoverable RecoveryStrategy | Fatal
  deriving (Show, Eq, Generic)

instance ToJSON ErrorSeverity
instance FromJSON ErrorSeverity

data RecoveryStrategy =
    Retry Int           -- Retry n times
  | RetryWithBackoff Int Int  -- Retry n times with backoff seconds
  | Fallback Text       -- Use fallback strategy
  | SkipAndContinue     -- Skip this operation
  | Reconnect           -- Reconnect to broker
  deriving (Show, Eq, Generic)

instance ToJSON RecoveryStrategy
instance FromJSON RecoveryStrategy

-- Error context with tracing
data ErrorContext = ErrorContext
  { ecCorrelationId :: Maybe CorrelationId
  , ecTimestamp :: UTCTime
  , ecComponent :: Maybe ComponentName
  , ecOperation :: Maybe Text
  , ecSeverity :: ErrorSeverity
  , ecStackTrace :: [Text]
  } deriving (Show, Generic)

instance ToJSON ErrorContext
instance FromJSON ErrorContext

-- Enhanced Result type
type Result a = Either TempehError a

data ResultWithContext a = ResultWithContext
  { rwcResult :: Either TempehError a
  , rwcContext :: Maybe ErrorContext
  } deriving (Show, Generic)

instance (ToJSON a) => ToJSON (ResultWithContext a)
instance (FromJSON a) => FromJSON (ResultWithContext a)

-- Legacy AppError for backward compatibility
data AppError =
    ValidationError Text
  | DataLoadError Text
  | StrategyInitError Text
  | ConfigError Text
  | InternalError Text
  deriving (Show, Eq)

instance Exception AppError

-- Convenience functions
withErrorContext :: ErrorContext -> Result a -> ResultWithContext a
withErrorContext ctx result = ResultWithContext result (Just ctx)

addErrorContext :: Text -> ErrorContext -> ErrorContext
addErrorContext operation ctx = ctx
  { ecOperation = Just operation
  , ecStackTrace = operation : ecStackTrace ctx
  }

isRecoverable :: TempehError -> Bool
isRecoverable err = case getRecoveryStrategy err of
  Just (Recoverable _) -> True
  _ -> False

getRecoveryStrategy :: TempehError -> Maybe ErrorSeverity
getRecoveryStrategy (DataError _) = Just (Recoverable (Retry 3))
getRecoveryStrategy (BrokerError details) =
  case bedHttpStatus details of
    Just 429 -> Just (Recoverable (RetryWithBackoff 3 5))  -- Rate limited
    Just 503 -> Just (Recoverable (RetryWithBackoff 2 10)) -- Service unavailable
    Just 401 -> Just (Recoverable Reconnect)               -- Auth expired
    _ -> Just Fatal
getRecoveryStrategy (ConfigurationError _) = Just Fatal
getRecoveryStrategy (SystemError _) = Just Fatal
getRecoveryStrategy _ = Just (Recoverable (Retry 1))

-- Error creation helpers
dataError :: Text -> TempehError
dataError msg = DataError $ DataErrorDetails msg Nothing Nothing Nothing Nothing

strategyError :: Text -> TempehError
strategyError msg = StrategyError $ StrategyErrorDetails msg Nothing Nothing Nothing

riskError :: Text -> TempehError
riskError msg = RiskError $ RiskErrorDetails msg Nothing Nothing Nothing Nothing

configError :: Text -> TempehError
configError msg = ConfigurationError $ ConfigErrorDetails msg Nothing Nothing Nothing

systemError :: Text -> TempehError
systemError msg = SystemError $ SystemErrorDetails msg Nothing Nothing Nothing

brokerError :: Text -> TempehError
brokerError msg = BrokerError $ BrokerErrorDetails msg Nothing Nothing Nothing Nothing

-- Additional error constructors for compatibility
fileError :: Text -> TempehError
fileError msg = DataError $ DataErrorDetails msg (Just "file") Nothing Nothing Nothing

unsupportedOperationError :: Text -> TempehError
unsupportedOperationError msg = SystemError $ SystemErrorDetails msg Nothing Nothing Nothing
