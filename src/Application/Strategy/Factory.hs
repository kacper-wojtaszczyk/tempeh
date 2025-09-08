{-# LANGUAGE OverloadedStrings #-}
module Application.Strategy.Factory
  ( initializeStrategyRegistry
  ) where

import Application.Strategy.Registry (StrategyRegistry, emptyRegistry, registerStrategy)
-- Import concrete strategies for their providers
import qualified Strategy.EmaCross as EmaCross
import qualified Strategy.RSI as RSI
import qualified Strategy.BollingerBands as BollingerBands
import qualified Strategy.TimingTest as TimingTest

-- Initialize registry with all available strategy providers (pure)
initializeStrategyRegistry :: StrategyRegistry
initializeStrategyRegistry =
  let registry = emptyRegistry
      registryWithEma = registerStrategy EmaCross.strategyProvider registry
      registryWithRsi = registerStrategy RSI.strategyProvider registryWithEma
      registryWithBollinger = registerStrategy BollingerBands.strategyProvider registryWithRsi
      registryWithTimingTest = registerStrategy TimingTest.strategyProvider registryWithBollinger
  in registryWithTimingTest
