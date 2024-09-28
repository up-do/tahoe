module SpecUtil where

import Hedgehog (Property, assert, diff, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty (
    TestTree,
    testGroup,
 )
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Hedgehog (testProperty)

import qualified Data.ByteString as BS

import Tahoe.Util (ceilDiv, chunkedBy)

tests :: TestTree
tests =
    testGroup
        "utilities"
        [ testProperty "BS.concat . chunkedBy n == id" prop_chunkedBy_identity
        , testProperty "The length of every result element, except sometimes the last, equals n" prop_chunkedBy_length
        , testCeilDiv
        ]

testCeilDiv :: TestTree
testCeilDiv = testCase "ceiling division" $ do
    assertEqual "evenly divisible" (ceilDiv 2 1) (2 :: Integer)
    assertEqual "needs rounding" (ceilDiv 3 2) (2 :: Integer)

prop_chunkedBy_identity :: Property
prop_chunkedBy_identity = property $ do
    someBytes <- forAll $ Gen.bytes (Range.linear 1 100)
    someSize <- forAll $ Gen.int (Range.linear 1 100)

    diff someBytes (==) (BS.concat . chunkedBy someSize $ someBytes)

prop_chunkedBy_length :: Property
prop_chunkedBy_length = property $ do
    someBytes <- forAll $ Gen.bytes (Range.linear 1 100)
    someSize <- forAll $ Gen.int (Range.linear 1 100)

    let chunks = chunkedBy someSize someBytes

        -- Handle the possibly-short element separately.
        equalSize = tail . reverse $ chunks
        short = last chunks

    -- All the rest should have the same length
    assert (all ((someSize ==) . BS.length) equalSize)

    -- And the last should just be no longer.
    assert (BS.length short <= someSize)
