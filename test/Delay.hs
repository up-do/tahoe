module Delay where

import Control.Concurrent.STM.Lifted (STM, TChan, TVar, newTChanIO, newTVarIO, readTChanIO, readTVar, readTVarIO, writeTChan, writeTChanIO)
import Tahoe.Storage.Backend.Internal.Delay (HasDelay (..), TimeoutOperation (Cancelled, Delayed))

data DelayState = DelayExpired | DelayCancelled | DelayActive deriving (Eq, Show)

data FakeDelay = FakeDelay
    { delayChan :: TChan TimeoutOperation
    , delayBackChan :: TChan Int
    , delayState :: TVar DelayState
    }

instance Show FakeDelay where
    show FakeDelay{} = "<FakeDelay>"

instance HasDelay FakeDelay where
    new = FakeDelay <$> newTChanIO <*> newTChanIO <*> newTVarIO DelayActive

    delay' FakeDelay{delayBackChan, delayState} duration = do
        readTVarIO delayState >>= \case
            DelayExpired -> error "Cannot delay' expired delay"
            DelayCancelled -> error "Cannot delay' cancelled delay"
            DelayActive -> fakeThreadDelay delayBackChan duration

    update FakeDelay{delayChan, delayState} duration = do
        readTVarIO delayState >>= \case
            DelayExpired -> error "Cannot update expired delay"
            DelayCancelled -> error "Cannot update cancelled delay"
            DelayActive -> writeTChanIO delayChan (Delayed duration)

    cancel FakeDelay{delayChan, delayState} = do
        readTVarIO delayState >>= \case
            DelayExpired -> error "Cannot cancel expired delay"
            DelayCancelled -> error "Cannot cancel cancelled delay"
            DelayActive -> writeTChanIO delayChan Cancelled

    wait FakeDelay{delayChan, delayState} = do
        readTVarIO delayState >>= \case
            DelayExpired -> error "Cannot wait expired delay"
            DelayCancelled -> error "Cannot wait cancelled delay"
            DelayActive -> readTChanIO delayChan

fakeThreadDelay :: TChan Int -> Int -> IO ()
fakeThreadDelay backChannel n = do
    elapsed <- readTChanIO backChannel
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
timePasses amount FakeDelay{delayBackChan} = do
    writeTChan delayBackChan amount

-- state <- readTVar tv
-- case state of
--     Unelapsed n
--         | n > amount -> writeTVar tv (Unelapsed $ n - amount)
--         | otherwise -> writeTVar tv Elapsed
--     Elapsed -> error "already expired"
--     Cancelled -> error "time cannot pass on cancelled Delay"
