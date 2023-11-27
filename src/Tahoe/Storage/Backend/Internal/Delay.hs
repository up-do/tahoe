module Tahoe.Storage.Backend.Internal.Delay where

import Control.Concurrent.STM.Lifted (TChan, newTChanIO, readTChanIO, writeTChanIO)
import GHC.Conc (threadDelay)

-- | A time-based delay.
class HasDelay a where
    -- | Create a new delay which will complete after a given number of
    -- microseconds.
    new :: IO a

    -- | Produce a result after a delay of a given number of microseconds.
    delay' :: a -> Int -> IO ()

    -- | Change the completion time for the delay to be the given number of
    -- microseconds from the current time.
    update :: a -> Int -> IO ()

    -- | Cancel a delay.  After a delay is cancelled, waiting on it or
    -- updating it causes an exception to be thrown.
    cancel :: a -> IO ()

    -- | Wait for some change in the delay configuration.
    wait :: a -> IO TimeoutOperation

-- | Represent a change to a scheduled delay.
data TimeoutOperation
    = -- | The delay is rescheduled to be a given number of microseconds after
      -- the current time.
      Delayed Int
    | -- | The delay is cancelled.
      Cancelled
    deriving (Show, Eq)

{- | A delay which uses ``threadDelay`` to let time pass and a ``TChan`` to
 pass control messages.
-}
instance HasDelay (TChan TimeoutOperation) where
    new = newTChanIO

    delay' _ = threadDelay

    update chan duration = writeTChanIO chan (Delayed duration)

    cancel chan = writeTChanIO chan Cancelled

    wait = readTChanIO
