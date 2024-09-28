{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Tahoe.CHK.Types where

import Data.Word (
    Word16,
 )

import qualified Data.ByteString as B
import Data.TreeDiff.Class (ToExpr)
import GHC.Generics (Generic)
import Tahoe.CHK.SHA256d (Digest')

-- 16 bytes
type StorageIndex = B.ByteString

-- How much data is there
type Size = Integer

-- Byte-based position into a share
type Offset = Integer

-- Segment-based position into a share
type SegmentNum = Int

-- With respect to FEC encoding, the number of a share.
type ShareNum = Int

-- The hash of a FEC-encoded block, parameterized on the hash algorithm.
type BlockHash a = Digest' a

-- The hash of some ciphertext, parameterized on the hash algorithm.
type CrypttextHash a = Digest' a

-- Erasure encoding / placement parameters
type Total = Word16
type Happy = ShareNum -- This is not like the others.
type Required = Word16
type SegmentSize = Size
data Parameters = Parameters
    { paramSegmentSize :: SegmentSize
    , paramTotalShares :: Total
    , paramHappyShares :: Happy
    , paramRequiredShares :: Required
    }
    deriving (Show, Ord, Eq, Generic, ToExpr)

requiredToInt :: Required -> Int
requiredToInt = fromIntegral

totalToInt :: Total -> Int
totalToInt = fromIntegral
