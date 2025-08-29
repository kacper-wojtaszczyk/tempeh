module Unit.Adapter.SimBrokerTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Adapter.SimBroker
import Port.Broker
import Domain.Types
import Data.Scientific (fromFloatDigits)
import Data.Text (Text, pack)

tests :: TestTree
tests = testGroup "SimBroker"
  [ testCase "Market order is recorded and account exists" $ do
      br <- newSimBroker (fromFloatDigits 1000)
      let order = Market EURUSD Buy (Qty (fromFloatDigits 1)) (Just (Price (fromFloatDigits 1.1)))
      oid <- bMarketOrder br order
      acc <- bGetAccount br
      ords <- readOrders br
      oid @?= OrderId 1
      aId acc @?= pack "SIM"
      -- ensure the stored order list contains what we placed
      case ords of
        ((oid', o) : _) -> do
          oid' @?= OrderId 1
        _ -> assertFailure "no orders recorded"
  ]
