{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- | Conversion between types with a known level of safety.  *Heavily* inspired
 by `witch` (which has dependencies that make it hard for us to use just yet).
-}
module Tahoe.SDMF.Internal.Converting where

import Data.Int (Int64)
import Data.Word (Word16, Word32, Word64, Word8)

-- | Precise, infallible conversion between two types.
class From a b where
    from :: a -> b

-- | Precise, fallible conversion between two types.
class TryFrom a b m where
    tryFrom ::
        -- | An error message for context if the conversion fails.
        String ->
        -- | The value to convert.
        a ->
        m b

instance MonadFail m => TryFrom Int Word32 m where
    tryFrom msg n
        | n < 0 = fail msg
        | n > maxWord32 = fail msg
        | otherwise = pure $ fromIntegral n
      where
        maxWord32 = from @Word32 @Int maxBound

instance MonadFail m => TryFrom Int Word64 m where
    tryFrom msg n
        | n < 0 = fail msg
        | otherwise = pure $ fromIntegral n

instance MonadFail m => TryFrom Int64 Word64 m where
    tryFrom msg n
        | n < 0 = fail msg
        | otherwise = pure $ fromIntegral n

instance From Word16 Int where
    from = fromIntegral

instance From Word8 Int where
    from = fromIntegral

instance From Word8 Word16 where
    from = fromIntegral

instance From Word32 Word64 where
    from = fromIntegral

instance From Word32 Int where
    from = fromIntegral

instance From Int64 Int where
    from = fromIntegral

instance From Int Int64 where
    from = fromIntegral

instance MonadFail m => TryFrom Word64 Int m where
    tryFrom msg n
        | n > maxInt = fail msg
        | otherwise = pure $ fromIntegral n
      where
        maxInt = fromIntegral (maxBound :: Int) :: Word64

instance MonadFail m => TryFrom Word16 Word8 m where
    tryFrom msg n
        | n > maxWord8 = fail msg
        | otherwise = pure $ fromIntegral n
      where
        maxWord8 = from @Word8 @Word16 maxBound

instance MonadFail m => TryFrom Word64 Int64 m where
    tryFrom msg n
        | n > maxInt64 = fail msg
        | otherwise = pure $ fromIntegral n
      where
        maxInt64 = fromIntegral (maxBound :: Int64) :: Word64

{- | Like `from` but with the order of the input/output type parameters
 reversed.
-}
into :: forall b a. From a b => a -> b
into = from

{- | Like `tryFrom` but with the order of the input/output type parameters
 reverse.
-}
tryInto :: forall b a m. TryFrom a b m => String -> a -> m b
tryInto = tryFrom
