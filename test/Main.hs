{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (
    guard,
    unless,
    void,
    when,
 )
import Prelude hiding (
    lookup,
    toInteger,
 )

import Data.Bits (
    xor,
 )

import GHC.Word (
    Word8,
 )

import qualified Data.Set as Set
import qualified Data.Text as T

import Control.Lens (view, (.~), (?~), (^.), _Just)
import Test.Hspec (
    Spec,
    SpecWith,
    around,
    context,
    describe,
    hspec,
    it,
    parallel,
    runIO,
    shouldBe,
    shouldReturn,
    shouldThrow,
 )
import Test.Hspec.Expectations (
    Selector,
 )

import Test.QuickCheck (
    Arbitrary (arbitrary),
    Gen,
    NonNegative (getNonNegative),
    Positive (getPositive),
    Property,
    Result (numDiscarded),
    forAll,
    property,
    shuffle,
    sublistOf,
 )

import Test.QuickCheck.Monadic (
    assert,
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
        createMutableStorageIndex,
        getImmutableShareNumbers,
        getMutableShareNumbers,
        readImmutableShare,
        writeImmutableShare
    ),
    ImmutableShareAlreadyWritten,
    writeMutableShare,
 )

import qualified Amazonka as AWS
import qualified Amazonka.S3 as S3
import Tahoe.Storage.Backend.S3 (
    S3Backend (S3Backend, s3BackendBucket, s3BackendEnv, s3BackendPrefix, s3BackendState),
    newS3Backend,
 )

-- We also get the Arbitrary ShareNumber instance from here.
import Lib (
    genStorageIndex,
 )

import Amazonka (runResourceT)
import Amazonka.S3.Lens (delete_objects, listObjectsResponse_contents, object_key)
import qualified Amazonka.S3.Lens as S3
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (readIORef)
import Data.Maybe (catMaybes)
import qualified Network.HTTP.Types as HTTP
import qualified System.IO as IO

main :: IO ()
main = hspec . parallel . describe "S3" $ spec

newtype ShareNumbers = ShareNumbers [ShareNumber] deriving (Eq, Ord, Show)

instance Arbitrary ShareNumbers where
    arbitrary = ShareNumbers . fmap ShareNumber <$> nums
      where
        nums =
            arbitrary
                >>= (shuffle . enumFromTo 0) . getNonNegative
                >>= \(num : rest) -> (num :) <$> sublistOf rest

spec :: Spec
spec = do
    Test.Hspec.context "filesystem" $
        Test.Hspec.around (withBackend s3Backend) storageSpec

-- The specification for a storage backend.
storageSpec :: (Backend b, b ~ S3Backend) => SpecWith b
storageSpec =
    context "v1" $ do
        context "immutable" $ do
            it "simple upload works" $ \backend -> do
                let storageIndex = "aaaaaaaaaaaaaaaa"
                    shareData = "Hello world"

                alloc <- createImmutableStorageIndex backend storageIndex (AllocateBuckets "" "" [ShareNumber 0] (fromIntegral $ Data.ByteString.length shareData))
                alloc `shouldBe` AllocationResult [] [ShareNumber 0]

                write <- writeImmutableShare backend storageIndex (ShareNumber 0) shareData Nothing
                write `shouldBe` ()

                readIORef (s3BackendState backend) `shouldReturn` mempty

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

instance Mess S3Backend where
    -- Delete all objects with a prefix matching this backend.  Leave the
    -- bucket alone in case there are other objects unrelated to this bucket
    -- in it.
    cleanup (S3Backend{s3BackendEnv, s3BackendBucket, s3BackendPrefix}) = runResourceT $ do
        resp <- AWS.send s3BackendEnv (S3.listObjects_prefix ?~ s3BackendPrefix $ S3.newListObjects s3BackendBucket)
        let objectKeys = catMaybes . traverse (S3.newObjectIdentifier . (^. object_key) <$>) $ resp ^. listObjectsResponse_contents

        unless (null objectKeys) . void $
            AWS.send s3BackendEnv (S3.newDeleteObjects s3BackendBucket (delete_objects .~ objectKeys $ S3.newDelete))

withBackend :: (Mess b, Backend b) => IO b -> ((b -> IO ()) -> IO ())
withBackend b action = do
    backend <- b
    action backend
    cleanup backend

-- In the result of creating an immutable storage index, the sum of
-- ``alreadyHave`` and ``allocated`` equals ``shareNumbers`` from the input.
alreadyHavePlusAllocatedImm ::
    Backend b =>
    b -> -- The backend on which to operate
    StorageIndex -> -- The storage index to use
    ShareNumbers -> -- The share numbers to allocate
    Size -> -- The size of each share
    Property
alreadyHavePlusAllocatedImm backend storageIndex (ShareNumbers shareNumbers) size = monadicIO $ do
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
    ShareNumbers -> -- The share numbers to allocate
    Size -> -- The size of each share
    Property
alreadyHavePlusAllocatedMut backend storageIndex (ShareNumbers shareNumbers) size = monadicIO $ do
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
    ShareNumbers ->
    ByteString ->
    Property
immutableWriteAndEnumerateShares backend storageIndex (ShareNumbers shareNumbers) shareSeed = monadicIO $ do
    let permutedShares = shareSeed <$ shareNumbers -- Prelude.map (permuteShare shareSeed) shareNumbers
    let size = fromIntegral (Data.ByteString.length shareSeed)
    let allocate = AllocateBuckets "renew" "cancel" shareNumbers size
    _result <- run $ createImmutableStorageIndex backend storageIndex allocate
    run $ writeShares (writeImmutableShare backend storageIndex) (zip shareNumbers permutedShares)
    readShareNumbers <- run $ getImmutableShareNumbers backend storageIndex
    when (readShareNumbers /= (CBORSet . Set.fromList $ shareNumbers)) $
        fail (show readShareNumbers ++ " /= " ++ (show . CBORSet . Set.fromList) shareNumbers)

-- Immutable share data written to the shares of a given storage index cannot
-- be rewritten by a subsequent writeImmutableShare operation.
immutableWriteAndRewriteShare ::
    Backend b =>
    b ->
    StorageIndex ->
    ShareNumbers ->
    ByteString ->
    Property
immutableWriteAndRewriteShare backend storageIndex (ShareNumbers shareNumbers) shareSeed = monadicIO $ do
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
    ShareNumbers ->
    ByteString ->
    Property
immutableWriteAndReadShare backend storageIndex (ShareNumbers shareNumbers) shareSeed = monadicIO $ do
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
    ShareNumbers ->
    ByteString ->
    Property
mutableWriteAndEnumerateShares backend storageIndex (ShareNumbers shareNumbers) shareSeed = monadicIO $ do
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

s3Backend :: IO S3Backend
s3Backend = runResourceT $ do
    let setLocalEndpoint = AWS.setEndpoint False "127.0.0.1" 9000
        setAddressingStyle s = s{AWS.s3AddressingStyle = AWS.S3AddressingStylePath}

    logger <- AWS.newLogger AWS.Debug IO.stdout

    env <- AWS.newEnv AWS.discover
    let loggedEnv = env -- {AWS.logger = logger}
        pathEnv = AWS.overrideService setAddressingStyle loggedEnv
        localEnv = AWS.overrideService setLocalEndpoint pathEnv
        env' = localEnv

    bucket <- AWS.sendEither env' (S3.newCreateBucket name)
    case bucket of
        -- AWS accepts duplicate create as long as you are the creator of the
        -- existing bucket.  MinIO returns a 409 Conflict response in that
        -- case.
        Left (AWS.ServiceError err) -> if HTTP.statusCode (err ^. AWS.serviceError_status) == 409 then pure () else error (show err)
        Left err -> error (show err)
        Right _ -> do
            exists <- AWS.await env' S3.newBucketExists (S3.newHeadBucket name)
            guard (exists == AWS.AcceptSuccess)

    prefix <- unusedPrefixFrom env' name $ T.singleton <$> ['a' .. 'z']
    liftIO $ newS3Backend env' name prefix
  where
    name = S3.BucketName "45336944-25bf-4fb1-8e82-9ba2631bda67"

    unusedPrefixFrom _ _ [] = error "Could not find an unused prefix"
    unusedPrefixFrom env bucket (letter : more) = do
        contents <- (^. listObjectsResponse_contents . _Just) <$> AWS.send env (S3.newListObjects bucket)
        let keys = T.pack . show . view object_key <$> contents
        if any (prefix `T.isPrefixOf`) keys
            then unusedPrefixFrom env bucket more
            else do
                -- Create a placeholder so we will immediately consider this
                -- prefix used.  This is still race-y still we look before we
                -- leap but the alternative is screwing with a bunch of
                -- complex AWS locking configuration that I don't feel like
                -- doing, especially just for this test suite which will
                -- mostly not have anything to race against anyway.
                void $ AWS.send env (S3.newPutObject bucket (S3.ObjectKey prefix) (AWS.toBody ("placeholder" :: String)))

                -- Then just return the letter and let the backend put in a
                -- separator if it wants to.
                pure letter
      where
        prefix = letter <> "/"
