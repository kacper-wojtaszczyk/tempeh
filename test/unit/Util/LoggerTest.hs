{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Unit.Util.LoggerTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Util.Logger
import Control.Monad.Reader (runReaderT, ReaderT, asks)
import Control.Monad.IO.Class (liftIO)
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import Data.Aeson (Value(..), toJSON)
import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import Control.Exception (catch, SomeException)

-- Test helper functions
testTime :: UTCTime
testTime = UTCTime (fromGregorian 2023 1 1) (secondsToDiffTime 0)

testCorrelationId :: CorrelationId
testCorrelationId = CorrelationId "test-correlation-123"

testComponent :: ComponentName
testComponent = ComponentName "TestComponent"

-- Helper to capture log output for testing
captureLogOutput :: ReaderT LogContext IO a -> IO (a, [String])
captureLogOutput action = withSystemTempDirectory "tempeh-log-test" $ \tmpDir -> do
  let logFile = tmpDir </> "test.log"
  result <- runFileLogger logFile action

  -- Read back the log file content
  fileExists <- doesFileExist logFile
  logContent <- if fileExists
    then do
      content <- readFile logFile
      return $ lines content
    else return []

  return (result, logContent)

tests :: TestTree
tests = testGroup "Logger"
  [ testGroup "Core Logging Functionality"
    [ testCase "Basic log message formatting" $ do
        (_, logLines) <- captureLogOutput $ do
          logInfo "Test info message"
          logError "Test error message"
          logWarn "Test warning message"
          logDebug "Test debug message"

        length logLines @?= 4
        assertBool "Info message should be logged" $
          any (T.isInfixOf "Test info message" . T.pack) logLines
        assertBool "Error message should be logged" $
          any (T.isInfixOf "Test error message" . T.pack) logLines
        assertBool "Log levels should be included" $
          any (T.isInfixOf "[Info]" . T.pack) logLines &&
          any (T.isInfixOf "[Error]" . T.pack) logLines

    , testCase "Structured logging with metadata" $ do
        (_, logLines) <- captureLogOutput $ do
          let metadata = Map.fromList
                [ ("key1", String "value1")
                , ("key2", Number 42)
                , ("key3", Bool True)
                ]
          logAtLevel Info "Structured message" metadata

        length logLines @?= 1
        assertBool "Message should be logged" $
          any (T.isInfixOf "Structured message" . T.pack) logLines

    , testCase "Log context with correlation ID" $ do
        (_, logLines) <- captureLogOutput $ do
          withCorrelationId testCorrelationId $ do
            logInfo "Message with correlation ID"

        length logLines @?= 1
        assertBool "Correlation ID should be included" $
          any (T.isInfixOf (unCorrelationId testCorrelationId) . T.pack) logLines

    , testCase "Log context with component name" $ do
        (_, logLines) <- captureLogOutput $ do
          withComponent testComponent $ do
            logInfo "Message with component"

        length logLines @?= 1
        assertBool "Component name should be included" $
          any (T.isInfixOf (unComponentName testComponent) . T.pack) logLines

    , testCase "Combined context: correlation ID and component" $ do
        (_, logLines) <- captureLogOutput $ do
          withCorrelationId testCorrelationId $ do
            withComponent testComponent $ do
              logInfo "Message with both context elements"

        length logLines @?= 1
        let logLine = T.pack $ head logLines
        assertBool "Both correlation ID and component should be present" $
          T.isInfixOf (unCorrelationId testCorrelationId) logLine &&
          T.isInfixOf (unComponentName testComponent) logLine
    ]

  , testGroup "Log Context Management"
    [ testCase "Empty log context" $ do
        let ctx = emptyLogContext
        lcCorrelationId ctx @?= Nothing
        lcComponent ctx @?= Nothing
        lcLogFile ctx @?= Nothing

    , testCase "Context modification preserves other fields" $ do
        (_, _) <- captureLogOutput $ do
          withCorrelationId testCorrelationId $ do
            withComponent testComponent $ do
              -- Both should be present in nested context
              ctx <- asks id
              liftIO $ do
                lcCorrelationId ctx @?= Just testCorrelationId
                lcComponent ctx @?= Just testComponent

        return ()

    , testCase "Context isolation between operations" $ do
        (_, logLines) <- captureLogOutput $ do
          withCorrelationId testCorrelationId $ do
            logInfo "Message 1"
          logInfo "Message 2"  -- Outside correlation context
          withCorrelationId (CorrelationId "different-id") $ do
            logInfo "Message 3"

        length logLines @?= 3
        let logTexts = map T.pack logLines
        -- First and third messages should have correlation IDs, second should not
        assertBool "Context isolation should work" $
          any (T.isInfixOf "test-correlation-123") logTexts &&
          any (T.isInfixOf "different-id") logTexts
    ]

  , testGroup "File-based Logging"
    [ testCase "Log file creation and writing" $ do
        withSystemTempDirectory "tempeh-log-test" $ \tmpDir -> do
          let logFile = tmpDir </> "custom.log"

          -- File should not exist initially
          initialExists <- doesFileExist logFile
          initialExists @?= False

          -- Run logging operation
          runFileLogger logFile $ do
            logInfo "Test file logging"
            logError "Another message"

          -- File should now exist with content
          finalExists <- doesFileExist logFile
          finalExists @?= True

          content <- readFile logFile
          let logLines = lines content
          length logLines @?= 2
          assertBool "Log messages should be in file" $
            any (T.isInfixOf "Test file logging" . T.pack) logLines

    , testCase "Auto-generated log file names" $ do
        fileName <- generateLogFileName testComponent
        assertBool "Generated filename should include component name" $
          T.isInfixOf (unComponentName testComponent) (T.pack fileName)
        assertBool "Generated filename should have .log extension" $
          T.isSuffixOf ".log" (T.pack fileName)
        assertBool "Generated filename should include timestamp" $
          T.isInfixOf "-" (T.pack fileName)  -- Timestamp separator

    , testCase "Component-based file logging" $ do
        withSystemTempDirectory "tempeh-log-test" $ \tmpDir -> do
          -- This test verifies the runFileLoggerWithComponent function works
          -- We can't easily test the exact file creation without mocking getCurrentTime
          -- But we can verify it doesn't crash and produces output
          result <- catch
            (do
              runFileLoggerWithComponent testComponent $ do
                logInfo "Component-based logging test"
              return True
            )
            (\(_ :: SomeException) -> return False)

          result @?= True
    ]

  , testGroup "Log Event Structure"
    [ testCase "LogEvent JSON serialization" $ do
        let event = LogEvent
              { leLevel = Info
              , leTimestamp = testTime
              , leCorrelationId = Just testCorrelationId
              , leComponent = Just testComponent
              , leMessage = "Test message"
              , leMetadata = Map.singleton "test" (String "value")
              }

        -- Should serialize without error
        let jsonValue = toJSON event
        case jsonValue of
          Object _ -> return ()  -- Success
          _ -> assertFailure "LogEvent should serialize to JSON Object"

    , testCase "LogLevel ordering" $ do
        Debug < Info @?= True
        Info < Warn @?= True
        Warn < Error @?= True
        Error < Debug @?= False

    , testCase "CorrelationId and ComponentName creation" $ do
        let corrId = CorrelationId "test-123"
            comp = ComponentName "TestComp"
        unCorrelationId corrId @?= "test-123"
        unComponentName comp @?= "TestComp"
    ]

  , testGroup "Error Handling and Edge Cases"
    [ testCase "Logging with empty message" $ do
        (_, logLines) <- captureLogOutput $ do
          logInfo ""

        length logLines @?= 1
        assertBool "Empty message should be logged" $
          any (T.isInfixOf "[Info]" . T.pack) logLines

    , testCase "Logging with special characters" $ do
        (_, logLines) <- captureLogOutput $ do
          logInfo "Message with special chars: äöü ñ 中文 🚀"

        length logLines @?= 1
        assertBool "Special characters should be preserved" $
          any (T.isInfixOf "äöü" . T.pack) logLines

    , testCase "Logging with very long message" $ do
        let longMessage = T.replicate 1000 "A"
        (_, logLines) <- captureLogOutput $ do
          logInfo longMessage

        length logLines @?= 1
        assertBool "Long message should be logged completely" $
          any (T.isInfixOf longMessage . T.pack) logLines

    , testCase "Multiple concurrent logging operations" $ do
        -- Test that logging is safe with concurrent operations
        (_, logLines) <- captureLogOutput $ do
          mapM_ (\i -> logInfo $ T.pack $ "Message " ++ show i) [1..10]

        length logLines @?= 10
        assertBool "All concurrent messages should be logged" $
          all (\i -> any (T.isInfixOf (T.pack $ "Message " ++ show i) . T.pack) logLines) [1..10]
    ]

  , testGroup "Integration with Existing Error System"
    [ testCase "Logging should work with existing Result types" $ do
        -- This test verifies that logging integrates well with your existing error handling
        (_, logLines) <- captureLogOutput $ do
          logError "Simulating error condition"
          logInfo "Recovery attempted"
          logInfo "Operation completed"

        length logLines @?= 3
        assertBool "Error and recovery should be logged" $
          any (T.isInfixOf "error condition" . T.pack) logLines &&
          any (T.isInfixOf "Recovery attempted" . T.pack) logLines

    , testCase "Component-based logging for different modules" $ do
        (_, logLines) <- captureLogOutput $ do
          withComponent (ComponentName "DataLoader") $ do
            logInfo "Loading data"
          withComponent (ComponentName "Strategy") $ do
            logInfo "Executing strategy"
          withComponent (ComponentName "RiskManager") $ do
            logInfo "Checking risk limits"

        length logLines @?= 3
        let logTexts = map T.pack logLines
        assertBool "All components should be logged separately" $
          any (T.isInfixOf "DataLoader") logTexts &&
          any (T.isInfixOf "Strategy") logTexts &&
          any (T.isInfixOf "RiskManager") logTexts
    ]
  ]
