{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Port.Broker where

import Domain.Types
import Control.Concurrent.STM (TChan)

-- Broker typeclass:
-- 'b' = broker handle type, 'm' = effect monad (IO, or test monad)
class Monad m => Broker b m | b -> m where
  bGetAccount  :: b -> m Account
  bMarketOrder :: b -> Order -> m OrderId
  bStreamTicks :: b -> Instrument -> m (TChan Tick)
