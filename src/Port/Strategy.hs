module Port.Strategy where

import Domain.Types
import Data.Functor.Identity

-- Pluggable strategy abstraction.
-- s = internal strategy state
-- m = monad used by step (Identity for pure strategies)
data Strategy s m = Strategy
  { initState :: s
  , step      :: s -> Candle -> m (s, Signal)
  }
