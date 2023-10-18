{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib (
    gen10String,
    genStorageIndex,
    positiveIntegers,
    b32encode,
    b32decode,
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
    suchThatMap,
    vectorOf,
 )

-- Get the Arbitrary ByteString instance.
import Test.QuickCheck.Instances.ByteString ()

import TahoeLAFS.Storage.API (
    ShareNumber,
    StorageIndex,
    shareNumber,
 )

gen10String :: Gen String
gen10String = vectorOf 10 arbitrary

gen10ByteString :: Gen ByteString
gen10ByteString =
    pack <$> vectorOf 10 (arbitrary :: Gen Word8)

genStorageIndex :: Gen StorageIndex
genStorageIndex = b32encode <$> gen10ByteString

positiveIntegers :: Gen Integer
positiveIntegers = abs <$> (arbitrary :: Gen Integer)

instance Arbitrary ShareNumber where
    arbitrary = suchThatMap positiveIntegers shareNumber

b32table :: ByteString
b32table = "abcdefghijklmnopqrstuvwxyz234567"

b32encode :: ByteString -> String
b32encode = Text.unpack . Base32.toText . Base32.fromBytes b32table

b32decode :: String -> ByteString
b32decode base32 =
    Base32.toBytes b32table $ Base32.fromText b32table $ Text.pack base32
