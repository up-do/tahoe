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
    Positive (..),
    Property,
    forAll,
    property,
 )

import Test.QuickCheck.Monadic (
    monadicIO,
    pre,
    run,
 )

import Data.ByteString (
    ByteString,
    length,
    map,
 )

import TahoeLAFS.Storage.API (
    AllocateBuckets (AllocateBuckets),
    AllocationResult (AllocationResult),
    CBORSet (..),
    LeaseSecret (..),
    ShareData,
    ShareNumber (ShareNumber),
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
        getImmutableShareNumbers,
        getMutableShareNumbers,
        readImmutableShare,
        writeImmutableShare
    ),
    WriteImmutableError (..),
    writeMutableShare,
 )

import Data.IORef (IORef)

-- We also get the Arbitrary ShareNumber instance from here.
import Lib (
    genStorageIndex,
 )

import TahoeLAFS.Storage.Backend.Memory (
    MemoryBackend (..),
    memoryBackend,
 )

import TahoeLAFS.Storage.Backend.Filesystem (
    FilesystemBackend (FilesystemBackend),
 )

isUnique :: Ord a => [a] -> Bool
isUnique xs = Prelude.length xs == Prelude.length (Set.toList $ Set.fromList xs)

-- XXX null ?
hasElements :: [a] -> Bool
hasElements = not . null

permuteShare :: ByteString -> ShareNumber -> ByteString
permuteShare seed number =
    Data.ByteString.map xor' seed
  where
    xor' :: Word8 -> Word8
    xor' = xor $ fromInteger $ toInteger number

writeShares ::
    (ShareNumber -> ShareData -> Maybe a -> IO ()) ->
    [(ShareNumber, ShareData)] ->
    IO ()
writeShares _write [] = return ()
writeShares write ((shareNumber, shareData) : rest) = do
    -- TODO For now we'll do single complete writes.  Later try breaking up the data.
    write shareNumber shareData Nothing
    writeShares write rest

alreadyHavePlusAllocatedImm ::
    (Backend b, Mess b) =>
    IO b -> -- The backend on which to operate
    StorageIndex -> -- The storage index to use
    [ShareNumber] -> -- The share numbers to allocate
    Positive Size -> -- The size of each share
    Property
alreadyHavePlusAllocatedImm makeBackend storageIndex shareNumbers (Positive size) = monadicIO $ do
    pre (isUnique shareNumbers)
    pre (hasElements shareNumbers)
    run $
        withBackend makeBackend $ \backend -> do
            result <- createImmutableStorageIndex backend storageIndex (Just [Upload "hello world"]) $ AllocateBuckets shareNumbers size
            when (alreadyHave result ++ allocated result /= shareNumbers) $
                fail
                    ( show (alreadyHave result)
                        ++ " ++ "
                        ++ show (allocated result)
                        ++ " /= "
                        ++ show shareNumbers
                    )

-- The share numbers of immutable share data written to the shares of a given
-- storage index can be retrieved.
immutableWriteAndEnumerateShares ::
    (Backend b, Mess b) =>
    IO b ->
    StorageIndex ->
    [ShareNumber] ->
    ByteString ->
    Property
immutableWriteAndEnumerateShares makeBackend storageIndex shareNumbers shareSeed = monadicIO $ do
    pre (isUnique shareNumbers)
    pre (hasElements shareNumbers)
    let permutedShares = Prelude.map (permuteShare shareSeed) shareNumbers
        size = fromIntegral (Data.ByteString.length shareSeed)
        allocate = AllocateBuckets shareNumbers size
    run $
        withBackend makeBackend $ \backend -> do
            void $ createImmutableStorageIndex backend storageIndex uploadSecret allocate
            writeShares (\sn -> writeImmutableShare backend storageIndex sn uploadSecret) (zip shareNumbers permutedShares)
            readShareNumbers <- getImmutableShareNumbers backend storageIndex
            when (readShareNumbers /= (CBORSet . Set.fromList $ shareNumbers)) $
                fail (show readShareNumbers ++ " /= " ++ show shareNumbers)
  where
    uploadSecret = Just [Upload "hello"]

-- Immutable share data written to the shares of a given storage index cannot
-- be rewritten by a subsequent writeImmutableShare operation.
immutableWriteAndRewriteShare ::
    (Backend b, Mess b) =>
    IO b ->
    StorageIndex ->
    [ShareNumber] ->
    ByteString ->
    Property
immutableWriteAndRewriteShare makeBackend storageIndex shareNumbers shareSeed = monadicIO $ do
    pre (isUnique shareNumbers)
    pre (hasElements shareNumbers)
    let size = fromIntegral (Data.ByteString.length shareSeed)
        allocate = AllocateBuckets shareNumbers size
        aShareNumber = head shareNumbers
        aShare = permuteShare shareSeed aShareNumber
    run $
        withBackend makeBackend $ \backend -> do
            void $ createImmutableStorageIndex backend storageIndex uploadSecret allocate
            let write = writeImmutableShare backend storageIndex aShareNumber uploadSecret aShare Nothing
            write
            write `shouldThrow` (== ImmutableShareAlreadyWritten)
  where
    uploadSecret = Just [Upload "hello"]

-- Immutable share data written to the shares of a given storage index can be
-- retrieved verbatim and associated with the same share numbers as were
-- specified during writing.
immutableWriteAndReadShare ::
    (Backend b, Mess b) =>
    IO b ->
    StorageIndex ->
    [ShareNumber] ->
    ByteString ->
    Property
immutableWriteAndReadShare makeBackend storageIndex shareNumbers shareSeed = monadicIO $ do
    pre (isUnique shareNumbers)
    pre (hasElements shareNumbers)
    let permutedShares = Prelude.map (permuteShare shareSeed) shareNumbers
    let size = fromIntegral (Data.ByteString.length shareSeed)
    let allocate = AllocateBuckets shareNumbers size
    run $
        withBackend makeBackend $ \backend -> do
            void $ createImmutableStorageIndex backend storageIndex uploadSecret allocate
            writeShares (\sn -> writeImmutableShare backend storageIndex sn uploadSecret) (zip shareNumbers permutedShares)
            readShares' <- mapM (\sn -> readImmutableShare backend storageIndex sn Nothing) shareNumbers
            when (permutedShares /= readShares') $
                fail (show permutedShares ++ " /= " ++ show readShares')
  where
    uploadSecret = Just [Upload "hello"]

-- The share numbers of mutable share data written to the shares of a given
-- storage index can be retrieved.
mutableWriteAndEnumerateShares ::
    (Backend b, Mess b) =>
    IO b ->
    StorageIndex ->
    [ShareNumber] ->
    ByteString ->
    Property
mutableWriteAndEnumerateShares makeBackend storageIndex shareNumbers shareSeed = monadicIO $ do
    pre (isUnique shareNumbers)
    pre (hasElements shareNumbers)
    let permutedShares = Prelude.map (permuteShare shareSeed) shareNumbers
    let nullSecrets =
            SlotSecrets
                { writeEnabler = ""
                , leaseRenew = ""
                , leaseCancel = ""
                }
    run $
        withBackend makeBackend $ \backend -> do
            writeShares (writeMutableShare backend nullSecrets storageIndex) (zip shareNumbers permutedShares)
            (CBORSet readShareNumbers) <- getMutableShareNumbers backend storageIndex
            when (readShareNumbers /= Set.fromList shareNumbers) $
                fail (show readShareNumbers ++ " /= " ++ show shareNumbers)

-- The specification for a storage backend.
storageSpec :: (Backend b, Mess b) => IO b -> Spec
storageSpec makeBackend =
    context "v1" $ do
        context "immutable" $ do
            describe "allocate a storage index" $
                it "accounts for all allocated share numbers" $
                    property $
                        forAll genStorageIndex (alreadyHavePlusAllocatedImm makeBackend)

        context "write a share" $ do
            it "disallows writes without an upload secret" $
                property $
                    withBackend makeBackend $ \backend -> do
                        AllocationResult [] [ShareNumber 0] <- createImmutableStorageIndex backend "storageindex" (Just [Upload "thesecret"]) (AllocateBuckets [ShareNumber 0] 100)
                        writeImmutableShare backend "storageindex" (ShareNumber 0) Nothing "fooooo" Nothing `shouldThrow` (== MissingUploadSecret)

            it "disallows writes without a matching upload secret" $
                property $
                    withBackend makeBackend $ \backend -> do
                        AllocationResult [] [ShareNumber 0] <- createImmutableStorageIndex backend "storageindex" (Just [Upload "thesecret"]) (AllocateBuckets [ShareNumber 0] 100)
                        -- Supply the wrong secret as an upload secret and the
                        -- right secret marked for some other use - this
                        -- should still fail.
                        writeImmutableShare backend "storageindex" (ShareNumber 0) (Just [Upload "wrongsecret"]) "fooooo" Nothing `shouldThrow` (== IncorrectUploadSecret)

            it "returns the share numbers that were written" $
                property $
                    forAll genStorageIndex (immutableWriteAndEnumerateShares makeBackend)

            it "returns the written data when requested" $
                property $
                    forAll genStorageIndex (immutableWriteAndReadShare makeBackend)

            it "cannot be written more than once" $
                property $
                    forAll genStorageIndex (immutableWriteAndRewriteShare makeBackend)

        context "mutable" $ do
            describe "write a share" $ do
                it "returns the share numbers that were written" $
                    property $
                        forAll genStorageIndex (mutableWriteAndEnumerateShares makeBackend)

spec :: Spec
spec = do
    Test.Hspec.context "memory" $ storageSpec memoryBackend

    Test.Hspec.context "filesystem" $ storageSpec filesystemBackend

filesystemBackend :: IO FilesystemBackend
filesystemBackend = do
    FilesystemBackend <$> createTemporaryDirectory

createTemporaryDirectory :: IO FilePath
createTemporaryDirectory = do
    parent <- getCanonicalTemporaryDirectory
    createTempDirectory parent "gbs-semanticspec"

class Mess a where
    -- Cleanup resources belonging to m
    cleanup :: a -> IO ()

instance Mess FilesystemBackend where
    cleanup (FilesystemBackend path) = removeDirectoryRecursive path

instance Mess (IORef MemoryBackend) where
    cleanup _ = pure ()

withBackend :: (Mess b, Backend b) => IO b -> ((b -> IO ()) -> IO ())
withBackend b action = do
    backend <- b
    action backend
    cleanup backend
