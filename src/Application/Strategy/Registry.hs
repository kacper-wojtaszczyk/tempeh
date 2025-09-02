{-# LANGUAGE OverloadedStrings #-}
module Application.Strategy.Registry
  ( StrategyProvider(..)
  , StrategyRegistry
  , emptyRegistry
  , registerStrategy
  , findStrategyByKeyword
  , createStrategyFromKeyword
  , listAvailableStrategies
  ) where

import qualified Data.Text as T
import Application.Strategy.Types (StrategyParameters(..), StrategyInstance, StrategyProvider(..), StrategyFactory)
import Data.List (find)

-- Abstract strategy registry that holds strategy providers
newtype StrategyRegistry = StrategyRegistry [StrategyProvider]

-- Create empty registry
emptyRegistry :: StrategyRegistry
emptyRegistry = StrategyRegistry []

-- Register a strategy provider (plugin-style)
registerStrategy :: StrategyProvider -> StrategyRegistry -> StrategyRegistry
registerStrategy provider (StrategyRegistry providers) =
  StrategyRegistry (provider : providers)

-- Lookup strategy provider by keyword
findStrategyByKeyword :: String -> StrategyRegistry -> Maybe StrategyProvider
findStrategyByKeyword keyword (StrategyRegistry providers) =
  find (\p -> T.pack keyword == spKeyword p) providers

-- Create strategy instance from keyword and optional parameters
createStrategyFromKeyword :: String -> Maybe [T.Text] -> StrategyRegistry -> Maybe StrategyInstance
createStrategyFromKeyword keyword maybeParams registry = do
  provider <- findStrategyByKeyword keyword registry
  let params = case maybeParams of
        Nothing -> spDefaultParams provider
        Just paramStrs -> case spParseParams provider paramStrs of
          Just parsedParams -> parsedParams
          Nothing -> spDefaultParams provider
  if spValidateParams provider params
    then Just $ spFactory provider params
    else Nothing

-- List all available strategies
listAvailableStrategies :: StrategyRegistry -> [StrategyProvider]
listAvailableStrategies (StrategyRegistry providers) = providers
