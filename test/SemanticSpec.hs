{-# LANGUAGE FlexibleInstances #-}

module SemanticSpec (
    spec,
) where

import Prelude hiding (
    lookup,
    toInteger,
 )

import Control.Monad (
    void,
    when,
 )

import Data.Bits (
    xor,
 )

import GHC.Word (
    Word8,
 )

import qualified Data.Map.Strict as Map
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
    context,
    describe,
    it,
    shouldBe,
    shouldThrow,
 )
import Test.QuickCheck (
    Gen,
    NonEmptyList (getNonEmpty),
    NonNegative (NonNegative),
    Positive (..),
    Property,
    chooseInteger,
    counterexample,
    forAll,
    ioProperty,
    oneof,
    property,
    vector,
    (==>),
 )

import Test.QuickCheck.Monadic (
    monadicIO,
    run,
 )

import qualified Data.ByteString as B

import TahoeLAFS.Storage.API (
    AllocateBuckets (AllocateBuckets),
    AllocationResult (AllocationResult),
    CBORSet (..),
    LeaseSecret (..),
    Offset,
    ReadTestWriteResult (readData, success),
    ReadTestWriteVectors,
    ReadVector (ReadVector),
    ShareData,
    ShareNumber (ShareNumber),
    Size,
    StorageIndex,
    TestWriteVectors,
    UploadSecret (UploadSecret),
    WriteEnablerSecret (WriteEnablerSecret),
    WriteVector (WriteVector),
    allocated,
    alreadyHave,
    readv,
    toInteger,
    writev,
 )

import TahoeLAFS.Storage.Backend (
    Backend (
        abortImmutableUpload,
        createImmutableStorageIndex,
        getImmutableShareNumbers,
        getMutableShareNumbers,
        readImmutableShare,
        readvAndTestvAndWritev,
        writeImmutableShare
    ),
    WriteImmutableError (..),
    writeMutableShare,
 )

import Data.IORef (IORef)

import TahoeLAFS.Storage.Backend.Memory (
    MemoryBackend (..),
    MutableShareSize (MutableShareSize),
    memoryBackend,
    shareDataSize,
    toMutableShareSize,
 )

import Data.Data (Proxy (Proxy))
import Data.Interval (Boundary (Closed, Open), Extended (Finite), Interval, interval, lowerBound, upperBound)
import qualified Data.IntervalSet as IS
import Tahoe.Storage.Testing.Spec (
    ShareNumbers (..),
    genStorageIndex,
    makeStorageSpec,
 )
import TahoeLAFS.Storage.Backend.Filesystem (
    FilesystemBackend (FilesystemBackend),
 )
import Test.QuickCheck.Classes (Laws (..), semigroupMonoidLaws)

-- | Create a Spec that checks the given Laws.
lawsCheck :: Laws -> Spec
lawsCheck Laws{lawsTypeclass, lawsProperties} =
    describe lawsTypeclass $
        mapM_ oneLawProp lawsProperties
  where
    oneLawProp (lawName, lawProp) = it lawName lawProp

spec :: Spec
spec = do
    context "utilities" $ do
        describe "MutableShareStorage" $ do
            it "finds the larger size for some cases" $ do
                toMutableShareSize (WriteVector 0 "x") <> toMutableShareSize (WriteVector 1 "x")
                    `shouldBe` MutableShareSize 0 2

                toMutableShareSize (WriteVector 0 "Hello") <> toMutableShareSize (WriteVector 1 "bye")
                    `shouldBe` MutableShareSize 0 5

                toMutableShareSize (WriteVector 0 "x") <> toMutableShareSize (WriteVector 3 "x")
                    `shouldBe` MutableShareSize 0 4

                toMutableShareSize (WriteVector 0 "Hello") <> toMutableShareSize (WriteVector 3 "world")
                    `shouldBe` MutableShareSize 0 8

        describe "shareDataSize" $ do
            it "converts list of WriteVector to a size" $ do
                shareDataSize [WriteVector 2 "foo", WriteVector 10 "quux"]
                    `shouldBe` 14
                shareDataSize [WriteVector 0 "foobar", WriteVector 2 "q"]
                    `shouldBe` 6
                shareDataSize []
                    `shouldBe` 0
                shareDataSize [WriteVector 2 "foo", WriteVector 3 "quux"]
                    `shouldBe` 7

        describe "TestWriteVectors"
            . lawsCheck
            . semigroupMonoidLaws
            $ (Proxy :: Proxy TestWriteVectors)

        describe "ReadTestWriteVectors"
            . lawsCheck
            . semigroupMonoidLaws
            $ (Proxy :: Proxy ReadTestWriteVectors)

    context "memory" $ makeStorageSpec memoryBackend cleanupMemory
    context "filesystem" $ makeStorageSpec filesystemBackend cleanupFilesystem

filesystemBackend :: IO FilesystemBackend
filesystemBackend = do
    FilesystemBackend <$> createTemporaryDirectory

createTemporaryDirectory :: IO FilePath
createTemporaryDirectory = do
    parent <- getCanonicalTemporaryDirectory
    createTempDirectory parent "gbs-semanticspec"

cleanupFilesystem (FilesystemBackend path) = removeDirectoryRecursive path
cleanupMemory _ = pure ()
