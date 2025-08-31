{-# LANGUAGE OverloadedStrings #-}
module Application.Migration where

import Application.Main (runBacktestWithNewArchitecture)
import Domain.Services.BacktestService (StrategyParameters(..))
import Domain.Types (Instrument(..))
import Port.DataProvider (DateRange(..))
import qualified Data.Text as T

-- Migration helper - now just demos the new architecture
demonstrateNewArchitecture :: IO ()
demonstrateNewArchitecture = do
  putStrLn "=== NEW PORTS & ADAPTERS ARCHITECTURE ==="
  putStrLn ""
  putStrLn "Architecture benefits:"
  putStrLn "✅ Dependency injection - easy to swap CSV for database"
  putStrLn "✅ Pure domain logic - business rules separated from infrastructure"
  putStrLn "✅ Testable components - each layer can be tested in isolation"
  putStrLn "✅ Extensible design - easy to add new strategies, data sources"
  putStrLn "✅ Type safety - compile-time guarantees about interfaces"
  putStrLn ""

  let instrument = Instrument "EURUSD"
      dateRange = DateRange 2025 7 2025 8
      strategyParams = EmaCrossParams 10 20 0.0001

  putStrLn "Running backtest with new architecture..."
  runBacktestWithNewArchitecture instrument dateRange strategyParams
