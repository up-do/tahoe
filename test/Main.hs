module Main (main) where

import Prelude hiding (
    lookup,
    toInteger,
 )

import Control.Monad (
    when,
 )

import Data.Bits (
    xor,
 )

import GHC.Word (
    Word8,
 )

import qualified Data.Set as Set

import System.Directory (
    removeDirectoryRecursive,
 )

import System.IO.Temp (
    createTempDirectory,
    getCanonicalTemporaryDirectory,
 )

import Test.Hspec (
    Spec,
    SpecWith,
    around,
    before,
    context,
    describe,
    it,
    shouldThrow,
 )
import Test.Hspec.Expectations (
    Selector,
 )

import Test.QuickCheck (
    Property,
    forAll,
    property,
 )

import Test.QuickCheck.Monadic (
    assert,
    monadicIO,
    pre,
    run,
 )

import Data.ByteString (
    ByteString,
    concat,
    length,
    map,
 )

import Data.List (
    sort,
 )

import TahoeLAFS.Storage.API (
    AllocateBuckets (AllocateBuckets),
    CBORSet (..),
    ShareData,
    ShareNumber,
    Size,
    SlotSecrets (..),
    StorageIndex,
    allocated,
    alreadyHave,
    toInteger,
 )

import TahoeLAFS.Storage.Backend (
    Backend (
        createImmutableStorageIndex,
        createMutableStorageIndex,
        getImmutableShareNumbers,
        getMutableShareNumbers,
        readImmutableShare,
        writeImmutableShare
    ),
    ImmutableShareAlreadyWritten,
    writeMutableShare,
 )

-- We also get the Arbitrary ShareNumber instance from here.
import Lib (
    genStorageIndex,
 )

import TahoeLAFS.Storage.Backend.Memory (
    MemoryBackend,
    memoryBackend,
 )

import TahoeLAFS.Storage.Backend.Filesystem (
    FilesystemBackend (FilesystemBackend),
 )

main :: IO ()
main = putStrLn "Test suite not yet implemented."

spec :: Spec
spec = do
    Test.Hspec.context "filesystem" $
        Test.Hspec.around (withBackend s3Backend) storageSpec

-- The specification for a storage backend.
storageSpec :: Backend b => SpecWith b
storageSpec =
    context "v1" $ do
        context "immutable" $ do
            describe "allocate a storage index" $
                it "accounts for all allocated share numbers" $ \backend ->
                    property $
                        forAll genStorageIndex (alreadyHavePlusAllocatedImm backend)

            context "write a share" $ do
                it "returns the share numbers that were written" $ \backend ->
                    property $
                        forAll genStorageIndex (immutableWriteAndEnumerateShares backend)

                it "returns the written data when requested" $ \backend ->
                    property $
                        forAll genStorageIndex (immutableWriteAndReadShare backend)

                it "cannot be written more than once" $ \backend ->
                    property $
                        forAll genStorageIndex (immutableWriteAndRewriteShare backend)

        context "mutable" $ do
            describe "allocate a storage index" $ do
                it "accounts for all allocated share numbers" $ \backend ->
                    property $
                        forAll genStorageIndex (alreadyHavePlusAllocatedMut backend)

            describe "write a share" $ do
                it "returns the share numbers that were written" $ \backend ->
                    property $
                        forAll genStorageIndex (mutableWriteAndEnumerateShares backend)

class Mess m where
    -- Cleanup resources belonging to m
    cleanup :: m -> IO ()

withBackend :: (Mess b, Backend b) => IO b -> ((b -> IO ()) -> IO ())
withBackend b action = do
    backend <- b
    action backend
    cleanup backend
