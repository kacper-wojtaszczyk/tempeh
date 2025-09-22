{-# LANGUAGE OverloadedStrings #-}

module Unit.Adapter.IG.ErrorTest (errorTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Time (getCurrentTime)
import Data.Text (Text)
import qualified Data.Text as T

import Adapter.IG.Error

errorTests :: TestTree
errorTests = testGroup "Adapter.IG.Error"
  [ testGroup "Error Classification"
    [ testCase "Authentication errors should be classified as critical" $ do
        now <- getCurrentTime
        let authError = IGError IGAuthenticationFailed "Auth failed" Critical Nothing now
        let severity = igErrorSeverity authError
        severity @?= Critical

    , testCase "Network errors should be classified as high severity" $ do
        now <- getCurrentTime
        let networkError = IGError IGNetworkError "Network timeout" High Nothing now
        let severity = igErrorSeverity networkError
        severity @?= High

    , testCase "Rate limit errors should be classified as medium severity" $ do
        now <- getCurrentTime
        let rateLimitError = IGError IGRateLimitExceeded "Rate limit exceeded" Medium Nothing now
        let severity = igErrorSeverity rateLimitError
        severity @?= Medium
    ]

  , testGroup "Recovery Strategy Determination"
    [ testCase "Session expired should suggest session renewal" $ do
        let strategy = determineRecoveryStrategy IGSessionExpired
        strategy @?= RefreshSession

    , testCase "Network errors should suggest retry with backoff" $ do
        let strategy = determineRecoveryStrategy IGNetworkError
        strategy @?= RetryWithBackoff

    , testCase "Authentication failures should suggest session refresh" $ do
        let strategy = determineRecoveryStrategy IGAuthenticationFailed
        strategy @?= RefreshSession
    ]

  , testGroup "Retry Logic"
    [ testCase "Network errors should allow retry" $ do
        now <- getCurrentTime
        let networkError = IGError IGNetworkError "Timeout" High Nothing now
        shouldRetry networkError 1 @?= True

    , testCase "Critical errors with high attempt count should not allow retry" $ do
        now <- getCurrentTime
        let criticalError = IGError IGAuthenticationFailed "Auth failed" Critical Nothing now
        shouldRetry criticalError 5 @?= False

    , testCase "Medium severity errors should allow retry" $ do
        now <- getCurrentTime
        let mediumError = IGError IGRateLimitExceeded "Rate limited" Medium Nothing now
        shouldRetry mediumError 1 @?= True
    ]

  , testGroup "Error Message Formatting"
    [ testCase "Error message should include code and message" $ do
        now <- getCurrentTime
        let error = IGError IGSessionExpired "Session has expired" High Nothing now
        let formatted = formatErrorMessage error
        T.isInfixOf "IGSessionExpired" formatted @?= True
        T.isInfixOf "Session has expired" formatted @?= True

    , testCase "Error message should include severity" $ do
        now <- getCurrentTime
        let error = IGError IGNetworkError "Connection timeout" High Nothing now
        let formatted = formatErrorMessage error
        T.isInfixOf "High" formatted @?= True
    ]

  , testGroup "Error Context"
    [ testCase "Error context should be created correctly" $ do
        let context = addErrorContext "test-operation" (Just "EURUSD") Nothing Nothing
        ecOperation context @?= "test-operation"
        ecInstrument context @?= Just "EURUSD"
    ]
  ]
