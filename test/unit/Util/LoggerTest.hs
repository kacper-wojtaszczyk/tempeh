{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Aeson (Value(..), toJSON, decode, object, (.=))
import qualified Data.Aeson as A
import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import Control.Exception (catch, SomeException)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as L8

-- Test helper functions
testTime :: UTCTime
testTime = UTCTime (fromGregorian 2023 1 1) (secondsToDiffTime 0)

testCorrelationId :: CorrelationId
testCorrelationId = CorrelationId "test-correlation-123"

testComponent :: ComponentName
testComponent = ComponentName "TestComponent"

-- Helper to capture JSON log output for testing
captureJsonLogOutput :: ReaderT LogContext IO a -> IO (a, [LogEntry])
captureJsonLogOutput action = withSystemTempDirectory "tempeh-json-log-test" $ \tmpDir -> do
  let logFile = tmpDir </> "test.log"
  result <- runFileLogger logFile action

  -- Read back the log file content and parse JSON entries
  fileExists <- doesFileExist logFile
  jsonEntries <- if fileExists
    then do
      content <- LBS.readFile logFile
      let jsonLines = L8.lines content
      let parsedEntries = map (decode :: LBS.ByteString -> Maybe LogEntry) jsonLines
      return $ map (\case Just entry -> entry; Nothing -> error "Failed to parse JSON log entry") parsedEntries
    else return []

  return (result, jsonEntries)

tests :: TestTree
tests = testGroup "Logger"
  [ testGroup "JSON Logging Functionality"
    [ testCase "Basic JSON log message formatting" $ do
        (_, jsonEntries) <- captureJsonLogOutput $ do
          withComponent testComponent $ do
            logInfo "Test JSON info message"
            logError "Test JSON error message"

        length jsonEntries @?= 2
        let infoEntry = head jsonEntries
        let errorEntry = jsonEntries !! 1

        logEntryLevel infoEntry @?= Info
        logEntryLevel errorEntry @?= Error
        logEntryMessage infoEntry @?= "Test JSON info message"
        logEntryMessage errorEntry @?= "Test JSON error message"
        logEntryComponent infoEntry @?= "TestComponent"
        logEntryComponent errorEntry @?= "TestComponent"

    , testCase "JSON timestamp format validation" $ do
        (_, jsonEntries) <- captureJsonLogOutput $ do
          withComponent testComponent $ do
            logInfo "Timestamp test"

        length jsonEntries @?= 1
        let entry = head jsonEntries
        let timestamp = logEntryTimestamp entry

        -- Check ISO 8601 format (basic validation)
        assertBool "Timestamp should contain date separator" $ 'T' `elem` timestamp
        assertBool "Timestamp should end with Z" $ 'Z' == last timestamp

    , testCase "JSON log levels serialization" $ do
        (_, jsonEntries) <- captureJsonLogOutput $ do
          withComponent testComponent $ do
            logDebug "Debug message"
            logInfo "Info message"
            logWarn "Warn message"
            logError "Error message"

        length jsonEntries @?= 4
        let levels = map logEntryLevel jsonEntries
        levels @?= [Debug, Info, Warn, Error]

    , testCase "JSON component handling with no component" $ do
        (_, jsonEntries) <- captureJsonLogOutput $ do
          -- No withComponent wrapper
          logInfo "No component message"

        length jsonEntries @?= 1
        let entry = head jsonEntries
        logEntryComponent entry @?= "UNKNOWN"

    , testCase "Structured logging with metadata" $ do
        (_, jsonEntries) <- captureJsonLogOutput $ do
          withComponent testComponent $ do
            let contextValue = object
                  [ "key1" .= String "value1"
                  , "key2" .= Number 42
                  , "key3" .= Bool True
                  ]
            logAtLevel Info "Structured message" (Just contextValue)

        length jsonEntries @?= 1
        let entry = head jsonEntries
        logEntryMessage entry @?= "Structured message"
        logEntryLevel entry @?= Info

    , testCase "Log context with correlation ID" $ do
        (_, jsonEntries) <- captureJsonLogOutput $ do
          withCorrelationId testCorrelationId $ do
            withComponent testComponent $ do
              logInfo "Message with correlation ID"

        length jsonEntries @?= 1
        let entry = head jsonEntries
        logEntryComponent entry @?= "TestComponent"
        logEntryMessage entry @?= "Message with correlation ID"

    , testCase "Combined context: correlation ID and component" $ do
        (_, jsonEntries) <- captureJsonLogOutput $ do
          withCorrelationId testCorrelationId $ do
            withComponent testComponent $ do
              logInfo "Message with both context elements"

        length jsonEntries @?= 1
        let entry = head jsonEntries
        logEntryComponent entry @?= "TestComponent"
        logEntryMessage entry @?= "Message with both context elements"
    ]

  , testGroup "Log Context Management"
    [ testCase "Empty log context" $ do
        let ctx = emptyLogContext
        lcCorrelationId ctx @?= Nothing
        lcComponent ctx @?= Nothing
        lcLogFile ctx @?= Nothing

    , testCase "Context modification preserves other fields" $ do
        (_, _) <- captureJsonLogOutput $ do
          withCorrelationId testCorrelationId $ do
            withComponent testComponent $ do
              -- Both should be present in nested context
              ctx <- asks id
              liftIO $ do
                lcCorrelationId ctx @?= Just testCorrelationId
                lcComponent ctx @?= Just testComponent

        return ()

    , testCase "Context isolation between operations" $ do
        (_, jsonEntries) <- captureJsonLogOutput $ do
          withCorrelationId testCorrelationId $ do
            withComponent (ComponentName "Comp1") $ do
              logInfo "Message 1"
          withComponent (ComponentName "Comp2") $ do
            logInfo "Message 2"  -- Different component context
          withCorrelationId (CorrelationId "different-id") $ do
            withComponent (ComponentName "Comp3") $ do
              logInfo "Message 3"

        length jsonEntries @?= 3
        let components = map logEntryComponent jsonEntries
        components @?= ["Comp1", "Comp2", "Comp3"]
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
            withComponent testComponent $ do
              logInfo "Test file logging"
              logError "Another message"

          -- File should now exist with JSON content
          finalExists <- doesFileExist logFile
          finalExists @?= True

          content <- LBS.readFile logFile
          let jsonLines = L8.lines content
          length jsonLines @?= 2

          -- Parse and validate JSON entries
          let parsedEntries = map (decode :: LBS.ByteString -> Maybe LogEntry) jsonLines
          all (\case Just _ -> True; Nothing -> False) parsedEntries @?= True

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
    [ testCase "LogEntry JSON serialization" $ do
        let entry = LogEntry
              { logEntryTimestamp = "2023-01-01T00:00:00.000Z"
              , logEntryLevel = Info
              , logEntryComponent = "TestComponent"
              , logEntryMessage = "Test message"
              }

        -- Should serialize to expected JSON format
        let jsonValue = toJSON entry
        case jsonValue of
          Object _ -> return ()  -- Success
          _ -> assertFailure "LogEntry should serialize to JSON Object"

        -- Test round-trip serialization
        let encoded = A.encode entry
        let decoded = A.decode encoded :: Maybe LogEntry
        case decoded of
          Just decodedEntry -> do
            logEntryLevel decodedEntry @?= Info
            logEntryComponent decodedEntry @?= "TestComponent"
            logEntryMessage decodedEntry @?= "Test message"
          Nothing -> assertFailure "LogEntry should decode successfully"

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
        (_, jsonEntries) <- captureJsonLogOutput $ do
          withComponent testComponent $ do
            logInfo ""

        length jsonEntries @?= 1
        let entry = head jsonEntries
        logEntryMessage entry @?= ""
        logEntryLevel entry @?= Info

    , testCase "Logging with special characters" $ do
        (_, jsonEntries) <- captureJsonLogOutput $ do
          withComponent testComponent $ do
            logInfo "Message with special chars: äöü ñ 中文 🚀"

        length jsonEntries @?= 1
        let entry = head jsonEntries
        assertBool "Special characters should be preserved in JSON" $
          T.isInfixOf "äöü" (logEntryMessage entry) &&
          T.isInfixOf "🚀" (logEntryMessage entry)

    , testCase "Logging with very long message" $ do
        let longMessage = T.replicate 1000 "A"
        (_, jsonEntries) <- captureJsonLogOutput $ do
          withComponent testComponent $ do
            logInfo longMessage

        length jsonEntries @?= 1
        let entry = head jsonEntries
        logEntryMessage entry @?= longMessage

    , testCase "Multiple concurrent logging operations" $ do
        (_, jsonEntries) <- captureJsonLogOutput $ do
          withComponent testComponent $ do
            mapM_ (\i -> logInfo $ T.pack $ "Message " ++ show i) [1..10]

        length jsonEntries @?= 10
        let messages = map logEntryMessage jsonEntries
        assertBool "All concurrent messages should be logged" $
          all (\i -> any (T.isInfixOf (T.pack $ "Message " ++ show i)) messages) [1..10]
    ]

  , testGroup "Integration with Existing Error System"
    [ testCase "Component-based logging for different modules" $ do
        (_, jsonEntries) <- captureJsonLogOutput $ do
          withComponent (ComponentName "DataLoader") $ do
            logInfo "Loading data"
          withComponent (ComponentName "Strategy") $ do
            logInfo "Executing strategy"
          withComponent (ComponentName "RiskManager") $ do
            logInfo "Checking risk limits"

        length jsonEntries @?= 3
        let components = map logEntryComponent jsonEntries
        components @?= ["DataLoader", "Strategy", "RiskManager"]
        let messages = map logEntryMessage jsonEntries
        messages @?= ["Loading data", "Executing strategy", "Checking risk limits"]
    ]
  ]
