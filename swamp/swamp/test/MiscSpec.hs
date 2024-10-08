module MiscSpec (
    spec,
) where

import Prelude hiding (
    toInteger,
 )

import Text.Printf (
    printf,
 )

import Test.Hspec (
    Spec,
    describe,
    it,
    shouldBe,
    shouldNotBe,
    shouldSatisfy,
 )

import qualified Data.ByteString as BS

import Test.QuickCheck (
    forAll,
    property,
 )

import TahoeLAFS.Storage.API (
    ShareNumber (ShareNumber),
    shareNumber,
    toInteger,
 )

import Lib (
    b32decode,
    b32encode,
    positiveIntegers,
 )
import Tahoe.Storage.Testing.Spec (genStorageIndex)
import TahoeLAFS.Storage.Backend.Filesystem (
    incomingPathOf,
    partitionM,
    pathOfShare,
    storageStartSegment,
 )

spec :: Spec
spec = do
    describe "partitionM" $
        it "handles empty lists" $
            partitionM (const $ pure True) ([] :: [(Integer, Integer)]) `shouldBe` Just ([], [])

    describe "partitionM" $
        it "puts matching elements in the first list and non-matching in the second" $
            partitionM (return . even) [5 :: Int, 5, 6, 7, 8, 8]
                `shouldBe` Just ([6, 8, 8], [5, 5, 7])

    describe "storageStartSegment" $
        it "returns a string of length 2" $
            property $
                forAll genStorageIndex (\storageIndex -> length (storageStartSegment storageIndex) `shouldBe` 2)

    describe "pathOfShare" $
        it "returns a path reflecting the storage index and share number" $
            property $
                forAll
                    genStorageIndex
                    ( \storageIndex shareNum ->
                        pathOfShare "/foo" storageIndex (ShareNumber shareNum)
                            `shouldBe` printf "/foo/shares/%s/%s/%d" (take 2 storageIndex) storageIndex shareNum
                    )

    describe "incomingPathOf" $
        it "returns a path reflecting the storage index and share number" $
            property $
                forAll
                    genStorageIndex
                    ( \storageIndex shareNum ->
                        incomingPathOf "/foo" storageIndex (ShareNumber shareNum)
                            `shouldBe` printf "/foo/shares/incoming/%s/%s/%d" (take 2 storageIndex) storageIndex shareNum
                    )

    describe "incomingPathOf vs pathOfShare" $
        it "returns different paths" $
            property $
                forAll
                    genStorageIndex
                    ( \storageIndex shareNum ->
                        let path = pathOfShare "/foo" storageIndex (ShareNumber shareNum)
                            incoming = incomingPathOf "/foo" storageIndex (ShareNumber shareNum)
                         in path `shouldNotBe` incoming
                    )

    describe "base32 round-trip" $
        it "b32encode and b32decode are inverses" $
            property $
                \bs -> (b32decode . b32encode) bs `shouldBe` bs

    describe "base32 alphabet" $
        it "encodes using only the base32 alphabet" $
            property $
                \bs -> b32encode bs `shouldSatisfy` onlyContains "abcdefghijklmnopqrstuvwxyz234567"

    describe "size ratio" $
        it "encodes to a string no more than twice the length" $
            property $
                \bs -> b32encode bs `shouldSatisfy` (\base32 -> length base32 <= 2 * BS.length bs)

-- Does the second list contain only elements of the first list?
onlyContains :: (Eq a) => [a] -> [a] -> Bool
onlyContains xs = foldr (\y -> (&&) (y `elem` xs)) True
