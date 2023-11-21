module Delay where

import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVar, retry, writeTVar)
import Tahoe.Storage.Backend.S3 (HasDelay (..))

data DelayDouble
  = Unelapsed Int
  | Elapsed
  | Cancelled
  deriving (Show, Eq, Ord)

instance HasDelay (TVar DelayDouble) where
    new = newTVarIO . Unelapsed
    wait d =
        readTVar d >>= \case
            Elapsed -> pure ()
            Unelapsed _ -> retry
            Cancelled -> error "waiting for cancelled Delay"
            -- "real" STM wait will simply wait forever though

    update d n =
        atomically $
            readTVar d >>= \case
                Elapsed -> error "updating elapsed delay"
                Unelapsed _ -> writeTVar d (Unelapsed n)
                Cancelled -> pure ()

    cancel d =
        atomically $
            readTVar d >>= \case
                Elapsed -> error "canceling elapsed delay"
                Unelapsed _ -> writeTVar d Cancelled
                Cancelled -> pure ()

-- Consume one of the "delay tokens" or expire the delay if none are
-- remaining.
timePasses :: Int -> TVar DelayDouble -> STM ()
timePasses amount tv = do
    state <- readTVar tv
    case state of
        Unelapsed n
            | n > amount -> writeTVar tv (Unelapsed $ n - amount)
            | otherwise -> writeTVar tv Elapsed
        Elapsed -> error "already expired"
        Cancelled -> error "time cannot pass on cancelled Delay"
