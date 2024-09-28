{-# LANGUAGE OverloadedStrings #-}

module Tahoe.Util where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as LBS

-- | The smallest multiple of `multiplier` which is >= `value`.
nextMultipleOf :: (Integral m, Integral v) => m -> v -> v
nextMultipleOf multiplier value = factor * fromIntegral multiplier
  where
    factor = factor' + (if remainder == 0 then 0 else 1)
    (factor', remainder) = value `divMod` fromIntegral multiplier

{- | Return the smallest integer which is a power of k and greater than or
 equal to n
-}
nextPowerOf :: (Ord p, Num p) => p -> p -> p
nextPowerOf k n =
    nextPowerOf' k n 1
  where
    nextPowerOf' k' n' p' =
        if p' < n'
            then nextPowerOf' k' n' (p' * k')
            else p'

{- | Construct a binary representation of the given integer.  The first
 argument represents a zero bit.  The second argument represents a one bit.
 The result is ordered from most to least significant bit.
-}
toBinary :: a -> a -> Int -> [a]
toBinary off on i = reverse $ toBinaryRev i
  where
    toBinaryRev 0 = []
    toBinaryRev n
        | n `mod` 2 == (0 :: Int) = off : toBinaryRev (n `div` 2)
        | otherwise = on : toBinaryRev (n `div` 2)

{- | Break up a byte string into equal sized pieces, except the last piece
 which might be short.  *BS.concat . chunkedBy n == id*
-}
chunkedBy ::
    -- | The number of bytes in each piece.
    Int ->
    -- | The byte string to break up.
    BS.ByteString ->
    [BS.ByteString]
chunkedBy _n "" = []
chunkedBy n xs = nextChunk : chunkedBy n theRest
  where
    (nextChunk, theRest) = BS.splitAt n xs

toStrictByteString :: BS.Builder -> BS.ByteString
toStrictByteString = LBS.toStrict . BS.toLazyByteString

-- | Integer division rounded towards positive infinity.
ceilDiv :: Integral i => i -> i -> i
ceilDiv a b = q + adjustment
  where
    (q, r) = a `divMod` b
    adjustment = case r of
        0 -> 0
        _ -> 1
