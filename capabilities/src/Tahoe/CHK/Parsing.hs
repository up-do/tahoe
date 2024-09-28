module Tahoe.CHK.Parsing where

import qualified Data.Set as Set
import Text.Megaparsec (ErrorFancy (ErrorFail), MonadParsec, fancyFailure)

-- | Parse an integral with lower and upper value constraints.
bounded ::
    (MonadParsec e s m, Ord n, Integral n) =>
    -- | A parser for an arbitrarily large integral value.
    m Integer ->
    -- | The smallest allowed value.
    n ->
    -- | The largest allowed value.
    n ->
    -- | A parser that succeeds only for integers within the given bounds.
    m n
bounded decimal low high = do
    -- Parse into an integer so there's no wrap-around
    v <- decimal
    if v < fromIntegral low
        then fancyFailure (Set.singleton (ErrorFail "below minimum allowed value"))
        else
            if v > fromIntegral high
                then fancyFailure (Set.singleton (ErrorFail "above maximum allowed value"))
                else pure (fromIntegral v)
