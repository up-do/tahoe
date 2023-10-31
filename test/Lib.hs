{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib (
    gen10String,
    genStorageIndex,
    positiveIntegers,
    b32encode,
    b32decode,
    ShareNumbers (..),
) where

import Data.Word (
    Word8,
 )

import qualified Data.Base32String as Base32

import Data.ByteString (
    ByteString,
    pack,
 )

import qualified Data.Text as Text

import Test.QuickCheck (
    Arbitrary (arbitrary),
    Gen,
    NonNegative (NonNegative, getNonNegative),
    shuffle,
    sublistOf,
    suchThatMap,
    vectorOf,
 )

-- Get the Arbitrary ByteString instance.
import Test.QuickCheck.Instances.ByteString ()

import TahoeLAFS.Storage.API (
    ShareNumber (..),
    StorageIndex,
    TestOperator (Eq),
    TestVector (TestVector),
    TestWriteVectors (TestWriteVectors),
    WriteVector (..),
 )

gen10String :: Gen String
gen10String = vectorOf 10 arbitrary

gen10ByteString :: Gen ByteString
gen10ByteString =
    suchThatMap (vectorOf 10 (arbitrary :: Gen Word8)) (Just . pack)

genStorageIndex :: Gen StorageIndex
genStorageIndex =
    suchThatMap gen10ByteString (Just . b32encode)

positiveIntegers :: Gen Integer
positiveIntegers =
    suchThatMap (arbitrary :: Gen Integer) (Just . abs)

b32table :: ByteString
b32table = "abcdefghijklmnopqrstuvwxyz234567"

b32encode :: ByteString -> String
b32encode = Text.unpack . Base32.toText . Base32.fromBytes b32table

b32decode :: String -> ByteString
b32decode base32 =
    Base32.toBytes b32table $ Base32.fromText b32table $ Text.pack base32

newtype ShareNumbers = ShareNumbers [ShareNumber] deriving (Eq, Ord, Show)

{- | An Arbitrary instance that guarantees ShareNumbers are unique and
   non-empty (without invoking discard).
-}
instance Arbitrary ShareNumbers where
    arbitrary = ShareNumbers . fmap ShareNumber <$> nums
      where
        nums =
            arbitrary
                >>= (shuffle . enumFromTo 0) . getNonNegative
                >>= \(num : rest) -> (num :) <$> sublistOf rest

instance Arbitrary TestWriteVectors where
    arbitrary = TestWriteVectors <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TestVector where
    arbitrary = TestVector <$> (getNonNegative <$> arbitrary) <*> (getNonNegative <$> arbitrary) <*> pure Eq <*> arbitrary

instance Arbitrary WriteVector where
    arbitrary = WriteVector <$> arbitrary <*> arbitrary
