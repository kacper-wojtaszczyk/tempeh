{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Error handling module - standardized error types and recovery strategies
module Adapter.IG.Error
  ( -- Error types
    IGError(..)
  , IGErrorCode(..)
  , ErrorSeverity(..)
  , RecoveryStrategy(..)
  -- Error handling
  , classifyError
  , determineRecoveryStrategy
  , shouldRetry
  , formatErrorMessage
  -- Error context
  , ErrorContext(..)
  , addErrorContext
  ) where

import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Aeson (ToJSON, FromJSON)

import Util.Error (TempehError(..))

-- | IG-specific error types
data IGError = IGError
  { igErrorCode :: IGErrorCode
  , igErrorMessage :: Text
  , igErrorSeverity :: ErrorSeverity
  , igErrorContext :: Maybe ErrorContext
  , igErrorTimestamp :: UTCTime
  } deriving (Show, Eq, Generic)

-- | Standardized IG error codes
data IGErrorCode
  = IGAuthenticationFailed
  | IGSessionExpired
  | IGInvalidRequest
  | IGRateLimitExceeded
  | IGMarketClosed
  | IGInsufficientFunds
  | IGNetworkError
  | IGServerError
  | IGInvalidInstrument
  | IGOrderRejected
  | IGPositionNotFound
  | IGUnknownError
  deriving (Show, Eq, Generic)

-- | Error severity levels
data ErrorSeverity
  = Critical    -- System cannot continue
  | High        -- Feature unavailable but system can continue
  | Medium      -- Degraded functionality
  | Low         -- Minor issue, system fully functional
  deriving (Show, Eq, Ord, Generic)

-- | Recovery strategies for different error types
data RecoveryStrategy
  = NoRecovery              -- Fatal error, cannot recover
  | RetryImmediate          -- Retry immediately
  | RetryWithBackoff        -- Retry with exponential backoff
  | RefreshSession          -- Re-authenticate and retry
  | SwitchToFallback        -- Use alternative data source/method
  | WaitAndRetry            -- Wait for condition to change (e.g., market open)
  deriving (Show, Eq, Generic)

-- | Error context information
data ErrorContext = ErrorContext
  { ecOperation :: Text
  , ecInstrument :: Maybe Text
  , ecConnectionId :: Maybe Text
  , ecRequestDetails :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON IGError
instance FromJSON IGError
instance ToJSON IGErrorCode
instance FromJSON IGErrorCode
instance ToJSON ErrorSeverity
instance FromJSON ErrorSeverity
instance ToJSON RecoveryStrategy
instance FromJSON RecoveryStrategy
instance ToJSON ErrorContext
instance FromJSON ErrorContext

-- Error classification and recovery logic
classifyError :: Text -> Int -> IGError -> IGError
classifyError operation httpStatus igError =
  case httpStatus of
    401 -> igError { igErrorCode = IGSessionExpired, igErrorSeverity = High }
    403 -> igError { igErrorCode = IGAuthenticationFailed, igErrorSeverity = Critical }
    429 -> igError { igErrorCode = IGRateLimitExceeded, igErrorSeverity = Medium }
    500 -> igError { igErrorCode = IGServerError, igErrorSeverity = High }
    503 -> igError { igErrorCode = IGServerError, igErrorSeverity = Medium }
    _   -> igError { igErrorCode = IGUnknownError, igErrorSeverity = Low }

determineRecoveryStrategy :: IGErrorCode -> RecoveryStrategy
determineRecoveryStrategy errorCode = case errorCode of
  IGAuthenticationFailed -> RefreshSession
  IGSessionExpired -> RefreshSession
  IGRateLimitExceeded -> RetryWithBackoff
  IGNetworkError -> RetryWithBackoff
  IGServerError -> RetryWithBackoff
  IGMarketClosed -> WaitAndRetry
  IGInvalidRequest -> NoRecovery
  IGInsufficientFunds -> NoRecovery
  IGInvalidInstrument -> NoRecovery
  IGOrderRejected -> NoRecovery
  IGPositionNotFound -> NoRecovery
  IGUnknownError -> RetryImmediate

shouldRetry :: IGError -> Int -> Bool
shouldRetry igError currentAttempt =
  case determineRecoveryStrategy (igErrorCode igError) of
    NoRecovery -> False
    RetryImmediate -> currentAttempt < 3
    RetryWithBackoff -> currentAttempt < 5
    RefreshSession -> currentAttempt < 2
    SwitchToFallback -> False
    WaitAndRetry -> currentAttempt < 10

formatErrorMessage :: IGError -> Text
formatErrorMessage igError =
  let baseMsg = igErrorMessage igError
      codeMsg = T.pack (show (igErrorCode igError))
      severityMsg = T.pack (show (igErrorSeverity igError))
  in "[" <> severityMsg <> "] " <> codeMsg <> ": " <> baseMsg

addErrorContext :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> ErrorContext
addErrorContext operation instrument connectionId requestDetails =
  ErrorContext operation instrument connectionId requestDetails
