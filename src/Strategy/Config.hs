{-# LANGUAGE DeriveGeneric #-}
module Strategy.Config where

import GHC.Generics (Generic)
import Data.Scientific (Scientific)

-- Configuration for different strategy types
data StrategyConfig = StrategyConfig
  { scType :: StrategyType
  , scParameters :: StrategyParameters
  } deriving (Show, Eq, Generic)

data StrategyType
  = EmaCrossover
  | MovingAverageRibbon
  | RSIMeanReversion
  deriving (Show, Eq, Generic)

-- Parameters for different strategies
data StrategyParameters
  = EmaCrossParams
    { ecpFastPeriod :: Int
    , ecpSlowPeriod :: Int
    , ecpSignalThreshold :: Scientific -- Minimum difference for signal
    }
  | RibbonParams
    { rpPeriods :: [Int]
    , rpMinAlignment :: Int -- Minimum aligned MAs for signal
    }
  | RSIParams
    { rsipPeriod :: Int
    , rsipOverbought :: Scientific
    , rsipOversold :: Scientific
    }
  deriving (Show, Eq, Generic)

-- Default strategy configurations
defaultEmaCrossConfig :: StrategyConfig
defaultEmaCrossConfig = StrategyConfig
  { scType = EmaCrossover
  , scParameters = EmaCrossParams
    { ecpFastPeriod = 5
    , ecpSlowPeriod = 20
    , ecpSignalThreshold = 0.0001 -- 1 pip threshold
    }
  }

-- More aggressive EMA config
aggressiveEmaCrossConfig :: StrategyConfig
aggressiveEmaCrossConfig = StrategyConfig
  { scType = EmaCrossover
  , scParameters = EmaCrossParams
    { ecpFastPeriod = 3
    , ecpSlowPeriod = 12
    , ecpSignalThreshold = 0.00005 -- 0.5 pip threshold
    }
  }

-- Conservative EMA config
conservativeEmaCrossConfig :: StrategyConfig
conservativeEmaCrossConfig = StrategyConfig
  { scType = EmaCrossover
  , scParameters = EmaCrossParams
    { ecpFastPeriod = 10
    , ecpSlowPeriod = 50
    , ecpSignalThreshold = 0.0002 -- 2 pip threshold
    }
  }
