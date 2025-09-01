{-# LANGUAGE OverloadedStrings #-}
module Util.Error where

import Data.Text (Text)
import qualified Data.Text as T

-- Application-specific error types
data AppError
  = ParseError Text
  | FileError Text
  | ConfigError Text
  | BacktestError Text
  | ValidationError Text
  | RiskViolationError Text
  | UnsupportedOperationError Text
  deriving (Show, Eq)

-- Helper to convert errors to readable messages
errorMessage :: AppError -> Text
errorMessage (ParseError msg) = "Parse error: " <> msg
errorMessage (FileError msg) = "File error: " <> msg
errorMessage (ConfigError msg) = "Configuration error: " <> msg
errorMessage (BacktestError msg) = "Backtest error: " <> msg
errorMessage (ValidationError msg) = "Validation error: " <> msg
errorMessage (RiskViolationError msg) = "Risk violation: " <> msg
errorMessage (UnsupportedOperationError msg) = "Unsupported operation: " <> msg

-- Result type alias for cleaner code
type Result a = Either AppError a

-- Helper functions for common operations
fromMaybe :: AppError -> Maybe a -> Result a
fromMaybe err Nothing = Left err
fromMaybe _ (Just x) = Right x

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x
