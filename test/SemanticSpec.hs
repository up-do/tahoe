module SemanticSpec (
    spec,
) where

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

-- In the result of creating an immutable storage index, the sum of
-- ``alreadyHave`` and ``allocated`` equals ``shareNumbers`` from the input.
alreadyHavePlusAllocatedImm ::
    Backend b =>
    b -> -- The backend on which to operate
    StorageIndex -> -- The storage index to use
    [ShareNumber] -> -- The share numbers to allocate
    Size -> -- The size of each share
    Property
alreadyHavePlusAllocatedImm backend storageIndex shareNumbers size = monadicIO $ do
    pre (isUnique shareNumbers)
    pre (hasElements shareNumbers)
    result <- run $ createImmutableStorageIndex backend storageIndex $ AllocateBuckets "renew" "cancel" shareNumbers size
    when (alreadyHave result ++ allocated result /= shareNumbers) $
        fail
            ( show (alreadyHave result)
                ++ " ++ "
                ++ show (allocated result)
                ++ " /= "
                ++ show shareNumbers
            )

-- In the result of creating a mutable storage index, the sum of
-- ``alreadyHave`` and ``allocated`` equals ``shareNumbers`` from the input.
alreadyHavePlusAllocatedMut ::
    Backend b =>
    b -> -- The backend on which to operate
    StorageIndex -> -- The storage index to use
    [ShareNumber] -> -- The share numbers to allocate
    Size -> -- The size of each share
    Property
alreadyHavePlusAllocatedMut backend storageIndex shareNumbers size = monadicIO $ do
    pre (isUnique shareNumbers)
    pre (hasElements shareNumbers)
    result <- run $ createMutableStorageIndex backend storageIndex $ AllocateBuckets "renew" "cancel" shareNumbers size
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
    Backend b =>
    b ->
    StorageIndex ->
    [ShareNumber] ->
    ByteString ->
    Property
immutableWriteAndEnumerateShares backend storageIndex shareNumbers shareSeed = monadicIO $ do
    pre (isUnique shareNumbers)
    pre (hasElements shareNumbers)
    let permutedShares = Prelude.map (permuteShare shareSeed) shareNumbers
    let size = fromIntegral (Data.ByteString.length shareSeed)
    let allocate = AllocateBuckets "renew" "cancel" shareNumbers size
    _result <- run $ createImmutableStorageIndex backend storageIndex allocate
    run $ writeShares (writeImmutableShare backend storageIndex) (zip shareNumbers permutedShares)
    readShareNumbers <- run $ getImmutableShareNumbers backend storageIndex
    when (readShareNumbers /= (CBORSet . Set.fromList $ shareNumbers)) $
        fail (show readShareNumbers ++ " /= " ++ show shareNumbers)

-- Immutable share data written to the shares of a given storage index cannot
-- be rewritten by a subsequent writeImmutableShare operation.
immutableWriteAndRewriteShare ::
    Backend b =>
    b ->
    StorageIndex ->
    [ShareNumber] ->
    ByteString ->
    Property
immutableWriteAndRewriteShare backend storageIndex shareNumbers shareSeed = monadicIO $ do
    pre (isUnique shareNumbers)
    pre (hasElements shareNumbers)
    let size = fromIntegral (Data.ByteString.length shareSeed)
    let allocate = AllocateBuckets "renew" "cancel" shareNumbers size
    let aShareNumber = head shareNumbers
    let aShare = permuteShare shareSeed aShareNumber
    let write =
            writeImmutableShare backend storageIndex aShareNumber aShare Nothing
    run $ do
        _ <- createImmutableStorageIndex backend storageIndex allocate
        write
        write `shouldThrow` (const True :: Selector ImmutableShareAlreadyWritten)

-- Immutable share data written to the shares of a given storage index can be
-- retrieved verbatim and associated with the same share numbers as were
-- specified during writing.
immutableWriteAndReadShare ::
    Backend b =>
    b ->
    StorageIndex ->
    [ShareNumber] ->
    ByteString ->
    Property
immutableWriteAndReadShare backend storageIndex shareNumbers shareSeed = monadicIO $ do
    pre (isUnique shareNumbers)
    pre (hasElements shareNumbers)
    let permutedShares = Prelude.map (permuteShare shareSeed) shareNumbers
    let size = fromIntegral (Data.ByteString.length shareSeed)
    let allocate = AllocateBuckets "renew" "cancel" shareNumbers size
    _result <- run $ createImmutableStorageIndex backend storageIndex allocate
    run $ writeShares (writeImmutableShare backend storageIndex) (zip shareNumbers permutedShares)
    readShares' <- run $ mapM (\sn -> readImmutableShare backend storageIndex sn Nothing) shareNumbers
    when (permutedShares /= readShares') $
        fail (show permutedShares ++ " /= " ++ show readShares')

-- The share numbers of mutable share data written to the shares of a given
-- storage index can be retrieved.
mutableWriteAndEnumerateShares ::
    Backend b =>
    b ->
    StorageIndex ->
    [ShareNumber] ->
    ByteString ->
    Property
mutableWriteAndEnumerateShares backend storageIndex shareNumbers shareSeed = monadicIO $ do
    pre (isUnique shareNumbers)
    pre (hasElements shareNumbers)
    let permutedShares = Prelude.map (permuteShare shareSeed) shareNumbers
    let size = fromIntegral (Data.ByteString.length shareSeed)
    let allocate = AllocateBuckets "renew" "cancel" shareNumbers size
    let nullSecrets =
            SlotSecrets
                { writeEnabler = ""
                , leaseRenew = ""
                , leaseCancel = ""
                }
    _result <- run $ createMutableStorageIndex backend storageIndex allocate
    run $ writeShares (writeMutableShare backend nullSecrets storageIndex) (zip shareNumbers permutedShares)
    (CBORSet readShareNumbers) <- run $ getMutableShareNumbers backend storageIndex
    when (readShareNumbers /= Set.fromList shareNumbers) $
        fail (show readShareNumbers ++ " /= " ++ show shareNumbers)

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

spec :: Spec
spec = do
    Test.Hspec.context "memory" $
        Test.Hspec.before memoryBackend storageSpec

    Test.Hspec.context "filesystem" $
        Test.Hspec.around (withBackend filesystemBackend) storageSpec

filesystemBackend :: IO FilesystemBackend
filesystemBackend = do
    FilesystemBackend <$> createTemporaryDirectory

createTemporaryDirectory :: IO FilePath
createTemporaryDirectory = do
    parent <- getCanonicalTemporaryDirectory
    createTempDirectory parent "gbs-semanticspec"

class Mess m where
    -- Cleanup resources belonging to m
    cleanup :: m -> IO ()

instance Mess FilesystemBackend where
    cleanup (FilesystemBackend path) = removeDirectoryRecursive path

instance Mess MemoryBackend where
    cleanup _ = return ()

withBackend :: (Mess b, Backend b) => IO b -> ((b -> IO ()) -> IO ())
withBackend b action = do
    backend <- b
    action backend
    cleanup backend
