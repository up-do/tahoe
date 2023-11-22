module Delay where

import Control.Concurrent.STM.Lifted (STM, TChan, newTChanIO, readTChanIO, writeTChan, writeTChanIO)
import Tahoe.Storage.Backend.Internal.Delay (HasDelay (..), TimeoutOperation (Cancelled, Delayed))

data FakeDelay = FakeDelay (TChan TimeoutOperation) (TChan Int)

instance Show FakeDelay where
    show (FakeDelay _ _) = "<FakeDelay>"

instance HasDelay FakeDelay where
    new = FakeDelay <$> newTChanIO <*> newTChanIO

    delay' (FakeDelay _ backChannel) = fakeThreadDelay backChannel

    update (FakeDelay chan _) duration = writeTChanIO chan (Delayed duration)

    cancel (FakeDelay chan _) = writeTChanIO chan Cancelled

    wait (FakeDelay chan _) = readTChanIO chan

fakeThreadDelay :: TChan Int -> Int -> IO ()
fakeThreadDelay backChannel n = do
    print "Going to read from backChannel"
    elapsed <- readTChanIO backChannel
    print $ "Want to wait " <> show n <> " microseconds, about to let " <> show elapsed <> "microseconds pass"
    if elapsed >= n then pure () else fakeThreadDelay backChannel (n - elapsed)

-- data DelayDouble
--     = Unelapsed Int
--     | Elapsed
--     | Cancelled
--     deriving (Show, Eq, Ord)

-- instance HasDelay (TVar DelayDouble) where
--     new = newTVarIO . Unelapsed
--     wait d =
--         readTVar d >>= \case
--             Elapsed -> pure ()
--             Unelapsed _ -> retry
--             Cancelled -> error "waiting for cancelled Delay"

--     -- "real" STM wait will simply wait forever though

--     update d n =
--         atomically $
--             readTVar d >>= \case
--                 Elapsed -> error "updating elapsed delay"
--                 Unelapsed _ -> writeTVar d (Unelapsed n)
--                 Cancelled -> pure ()

--     cancel d =
--         atomically $
--             readTVar d >>= \case
--                 Elapsed -> error "canceling elapsed delay"
--                 Unelapsed _ -> writeTVar d Cancelled
--                 Cancelled -> pure ()

-- Consume one of the "delay tokens" or expire the delay if none are
-- remaining.
timePasses :: Int -> FakeDelay -> STM ()
timePasses amount (FakeDelay _ backChannel) = do
    writeTChan backChannel amount

-- state <- readTVar tv
-- case state of
--     Unelapsed n
--         | n > amount -> writeTVar tv (Unelapsed $ n - amount)
--         | otherwise -> writeTVar tv Elapsed
--     Elapsed -> error "already expired"
--     Cancelled -> error "time cannot pass on cancelled Delay"
