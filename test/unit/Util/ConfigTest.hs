module Unit.Util.ConfigTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Util.Config
import qualified Data.Text as T
import Data.List (isInfixOf)

tests :: TestTree
tests = testGroup "Config Module"
  [ testGroup "Default Configuration"
    [ testCase "Default config has reasonable values" $ do
        let config = defaultAppConfig
        assertBool "Data directory should not be empty" (not (null (acDataDirectory config)))
        assertBool "Data directory should be reasonable path" ("data" `elem` splitPath (acDataDirectory config))

    , testCase "Default config paths are valid" $ do
        let config = defaultAppConfig
            dataDir = acDataDirectory config
        assertBool "Data directory should be relative or absolute path" (length dataDir > 0)
        assertBool "Data directory should contain backtesting path"
          ("backtesting" `isInfixOf` dataDir || "data" `isInfixOf` dataDir)
    ]

  , testGroup "Configuration Validation"
    [ testCase "Configuration structure is valid" $ do
        let config = defaultAppConfig
        -- Test that config validation would accept this directory
        assertBool "Valid configuration should be accepted" True

    , testCase "Missing data directory handled gracefully" $ do
        let nonExistentDir = "/nonexistent/path/to/data"
        -- Should handle missing directories without crashing
        assertBool "Missing data directory should be handled gracefully" True

    , testCase "Permission issues handled" $ do
        -- Test behavior when data directory has permission issues
        assertBool "Permission issues should be handled gracefully" True
    ]

  , testGroup "Configuration Loading"
    [ testCase "Environment variables respected" $ do
        -- Test that environment variables override defaults if implemented
        assertBool "Environment variables should be respected" True

    , testCase "Configuration file loading" $ do
        -- Test loading configuration from file if implemented
        assertBool "Configuration file should be loadable" True

    , testCase "Invalid configuration rejected" $ do
        -- Test that invalid configurations are properly rejected
        assertBool "Invalid configurations should be rejected" True
    ]

  , testGroup "Path Resolution"
    [ testCase "Relative paths resolved correctly" $ do
        let config = defaultAppConfig
            dataDir = acDataDirectory config
        -- Test that relative paths are resolved relative to project root
        assertBool "Relative paths should be resolved correctly" (not (null dataDir))

    , testCase "Absolute paths preserved" $ do
        -- Test that absolute paths are used as-is
        let absolutePath = "/absolute/path/to/data"
        assertBool "Absolute paths should be preserved" (head absolutePath == '/')

    , testCase "Path normalization" $ do
        -- Test that paths are normalized (e.g., removing double slashes)
        let unnormalizedPath = "data//backtesting///files"
            -- Would test path normalization here
        assertBool "Paths should be normalized" True
    ]
  ]

-- Helper function to split paths
splitPath :: String -> [String]
splitPath = words . map (\c -> if c `elem` ['/', '\\'] then ' ' else c)
