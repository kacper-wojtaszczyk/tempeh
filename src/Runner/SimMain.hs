module Runner.SimMain (runSim) where

import Port.Strategy
import Strategy.EmaCross
import Adapter.SimBroker
import Port.Broker
import Domain.Types
import Data.Functor.Identity
import Util.FakeData
import Data.Scientific (fromFloatDigits)
import Control.Monad (void)

-- Run a simulation: create a sim-broker, run the strategy over fake candles,
-- place Market orders on Enter signals.
runSim :: IO ()
runSim = do
  broker <- newSimBroker (fromFloatDigits 10000)
  let candles = fakeCandles 120
      strat = emaCrossStrategy 5 20

  putStrLn "Starting sim run..."
  loop broker strat (initState strat) candles

  acc <- bGetAccount broker
  putStrLn $ "Simulation finished. Account: " ++ show acc

  where
    loop _ _ _ [] = pure ()
    loop br s st (c:cs) = do
      let (newSt, sig) = runIdentity (step s st c)
      case sig of
        Enter side -> do
          let order = Market EURUSD side (Qty (fromFloatDigits 1000)) (Just (cClose c))
          _oid <- bMarketOrder br order
          putStrLn $ "Placed order at " ++ show (cTime c) ++ " -> " ++ show side
          loop br s newSt cs
        _ -> loop br s newSt cs
