{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
module Application.Strategy.Types where

import Domain.Types (Signal, Candle)
import Domain.Strategy (StrategyState)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)

-- Application layer types for strategy management - now fully abstract

-- Abstract strategy parameters that can hold any strategy's parameters
data StrategyParameters = forall a. (Show a, Eq a, Typeable a) => StrategyParameters
  { spStrategyType :: Text
  , spParameters :: a
  , spValidator :: a -> Bool
  , spParser :: [Text] -> Maybe a
  , spDefaults :: a
  }

instance Show StrategyParameters where
  show (StrategyParameters sType params _ _ _) =
    T.unpack sType ++ "(" ++ show params ++ ")"

instance Eq StrategyParameters where
  (StrategyParameters type1 _ _ _ _) == (StrategyParameters type2 _ _ _ _) = type1 == type2

-- Abstract strategy instance
data StrategyInstance = StrategyInstance
  { siName :: Text
  , siDescription :: Text
  , siParameters :: StrategyParameters
  , siSignalGenerator :: SignalGenerator
  , siInitialState :: StrategyState
  }

-- Signal generator function type
type SignalGenerator = [Candle] -> StrategyState -> (Signal, StrategyState)

-- Strategy factory function type - abstracts away concrete strategy creation
type StrategyFactory = StrategyParameters -> StrategyInstance

-- Abstract strategy provider interface
data StrategyProvider = StrategyProvider
  { spKeyword :: Text
  , spName :: Text
  , spDescription :: Text
  , spFactory :: StrategyFactory
  , spDefaultParams :: StrategyParameters
  , spParseParams :: [Text] -> Maybe StrategyParameters
  , spValidateParams :: StrategyParameters -> Bool
  }
