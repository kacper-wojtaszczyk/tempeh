{-# LANGUAGE DeriveGeneric #-}
module Util.Config where

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Scientific (Scientific, fromFloatDigits)
import Domain.Types (Instrument(..), Price(..))
import qualified Data.Text as T

-- Application configuration
data AppConfig = AppConfig
  { acDataDirectory :: FilePath
  , acBacktest :: BacktestConfig
  , acLogging :: LogConfig
  } deriving (Show, Generic)

-- Backtesting configuration
data BacktestConfig = BacktestConfig
  { bcInitialBalance :: Scientific
  , bcPositionSize :: Scientific
  , bcDefaultInstrument :: Instrument
  , bcCommission :: Scientific
  , bcSlippage :: Scientific
  } deriving (Show, Generic)

-- Logging configuration
data LogConfig = LogConfig
  { lcLevel :: LogLevel
  , lcOutputFile :: Maybe FilePath
  } deriving (Show, Generic)

data LogLevel = Debug | Info | Warn | Error
  deriving (Show, Eq, Ord, Generic)

-- Smart constructor for default configuration
defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig
  { acDataDirectory = "data/backtesting"
  , acBacktest = defaultBacktestConfig
  , acLogging = defaultLogConfig
  }

defaultBacktestConfig :: BacktestConfig
defaultBacktestConfig = BacktestConfig
  { bcInitialBalance = fromFloatDigits 10000.0
  , bcPositionSize = fromFloatDigits 1000.0
  , bcDefaultInstrument = Instrument (T.pack "EURUSD")
  , bcCommission = fromFloatDigits 0.0001  -- 1 pip
  , bcSlippage = fromFloatDigits 0.0001
  }

defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig
  { lcLevel = Info
  , lcOutputFile = Nothing
  }
