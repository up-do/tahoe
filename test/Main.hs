{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Amazonka (runResourceT)
import qualified Amazonka as AWS
import qualified Amazonka.S3 as S3
import Amazonka.S3.Lens (delete_objects, listObjectsResponse_contents, object_key)
import qualified Amazonka.S3.Lens as S3
import Control.Concurrent.STM (STM, TVar, atomically, newTVar, newTVarIO, readTVar, readTVarIO, retry, writeTVar)
import qualified Control.Concurrent.STM.Map as SMap
import Control.Lens (view, (.~), (?~), (^.), _Just)
import Control.Monad (
    guard,
    unless,
    void,
    (<=<),
 )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (catMaybes, isJust)
import qualified Data.Text as T
import Delay (FakeDelay (..), timePasses)
import Network.HTTP.Types (ByteRange (ByteRangeFromTo))
import qualified Network.HTTP.Types as HTTP
import qualified System.IO as IO
import Tahoe.Storage.Backend (
    AllocateBuckets (AllocateBuckets),
    AllocationResult (AllocationResult),
    Backend (..),
    LeaseSecret (Upload),
    ShareNumber (ShareNumber),
    StorageIndex,
    UploadSecret (UploadSecret),
    WriteImmutableError (ImmutableShareAlreadyWritten, ShareNotAllocated),
    WriteVector (WriteVector),
 )
import Tahoe.Storage.Backend.Internal.Delay (
    HasDelay (..),
 )
import Tahoe.Storage.Backend.S3 (
    S3Backend (..),
    UploadState (uploadProgressTimeout),
    applyWriteVectors,
    immutableUploadProgressTimeout,
    newS3Backend,
 )
import Tahoe.Storage.Testing.Spec (
    makeStorageSpec,
 )
import Test.Hspec (
    Spec,
    context,
    describe,
    example,
    hspec,
    it,
    parallel,
    shouldBe,
    shouldReturn,
    shouldSatisfy,
    shouldThrow,
 )

main :: IO ()
main = hspec . parallel . describe "S3" $ spec

spec :: Spec
spec = do
    context "utilities" $ do
        describe "applyWriteVectors" $ do
            it "replaces existing bytes with new bytes" $ do
                applyWriteVectors "abc" [WriteVector 0 "x", WriteVector 1 "y", WriteVector 2 "z"] `shouldBe` "xyz"
            it "can extend the length of the result" $ do
                applyWriteVectors "abc" [WriteVector 3 "xyz"] `shouldBe` "abcxyz"
            it "fills gaps with NULs" $ do
                applyWriteVectors "abc" [WriteVector 5 "xyz"] `shouldBe` "abc\0\0xyz"
            it "applies writes left-to-right" $ do
                applyWriteVectors "abc" [WriteVector 0 "x", WriteVector 0 "y"] `shouldBe` "ybc"
            it "accepts partial overlaps" $ do
                applyWriteVectors "abc" [WriteVector 0 "xy", WriteVector 1 "zw"] `shouldBe` "xzw"

    context "backend" $ do
        describe "immutable uploads" $ do
            it "timeout when progress is not being made" $ do
                let storageIndex = "abcd"
                    secrets = Just [Upload (UploadSecret "hello")]
                    shareNums = ShareNumber <$> [1, 2, 3]
                    allocate = AllocateBuckets shareNums 123

                backend <- s3Backend

                -- Allocate some stuff
                createImmutableStorageIndex backend storageIndex secrets allocate
                    `shouldReturn` AllocationResult [] shareNums

                -- Make sure we start off in a good state with respect to the timeout.
                delay <- atomically $ lookupDelay (storageIndex, ShareNumber 1) backend
                delay `shouldSatisfy` isJust

                -- Now force the expiration, as if too much time had passed with no progress.
                atomically $ s3TimePasses immutableUploadProgressTimeout (storageIndex, ShareNumber 1) backend

                -- And ensure you can no longer attempt writes to this share.
                writeImmutableShare backend storageIndex (ShareNumber 1) secrets "Hello world" Nothing
                    `shouldThrow` (== ShareNotAllocated)

            it "timeout is cancelled when upload cancelled" $ do
                let storageIndex = "abcd"
                    secrets = Just [Upload (UploadSecret "hello")]
                    shareNums = ShareNumber <$> [1, 2, 3]
                    allocate = AllocateBuckets shareNums 123
                backend <- s3Backend

                -- Allocate some stuff
                createImmutableStorageIndex backend storageIndex secrets allocate
                    `shouldReturn` AllocationResult [] shareNums

                -- remember the upload-state for this upload
                Just state <- atomically (SMap.lookup (storageIndex, ShareNumber 1) (s3BackendState backend))

                -- And ensure you can no longer attempt writes to this share.
                abortImmutableUpload backend storageIndex (ShareNumber 1) secrets

                -- the Delay should be cancelled so if we let the progress
                -- timeout elapse now, nothing should happen.  in particular,
                -- no exception should come out of the cleanup code telling us
                -- it couldn't find the state.
                atomically $ timePasses immutableUploadProgressTimeout (uploadProgressTimeout state)

            it "does not timeout when progress is being made" $ do
                let storageIndex = "abcd"
                    secrets = Just [Upload (UploadSecret "hello")]
                    shareNums = ShareNumber <$> [1, 2, 3]
                    allocate = AllocateBuckets shareNums 123
                backend <- s3Backend

                -- Allocate some stuff
                print "First"
                createImmutableStorageIndex backend storageIndex secrets allocate
                    `shouldReturn` AllocationResult [] shareNums

                -- Allow less than the timeout to pass
                print "Second"
                atomically $ s3TimePasses (immutableUploadProgressTimeout `div` 3 * 2) (storageIndex, ShareNumber 1) backend

                -- Then make some progress
                print "Third"
                writeImmutableShare backend storageIndex (ShareNumber 1) secrets "Hello world" (Just [ByteRangeFromTo 0 10])

                -- Then allow more time to pass such that the total elapsed time exceeds the progress timeout.
                print "Fourth"
                atomically $ s3TimePasses (immutableUploadProgressTimeout `div` 3 * 2) (storageIndex, ShareNumber 1) backend

                -- Then make some more progress.  This is allowed because the
                -- full progress timeout never elapsed without some progress
                -- being made.
                print "Fifth" -- XXX It explodes here
                writeImmutableShare backend storageIndex (ShareNumber 1) secrets "Goodbye world" (Just [ByteRangeFromTo 11 23])
                print "Sixth"

        describe "S3Backend" $ makeStorageSpec s3Backend cleanupS3

-- Consume one of the "delay tokens" or expire the delay if none are
-- remaining.
s3TimePasses :: Int -> (StorageIndex, ShareNumber) -> S3Backend FakeDelay -> STM ()
s3TimePasses amount key S3Backend{..} = do
    maybeDelay <- fmap uploadProgressTimeout <$> SMap.lookup key s3BackendState
    case maybeDelay of
        Nothing -> error "no such thing to expire"
        Just tv -> timePasses amount tv

lookupDelay :: (StorageIndex, ShareNumber) -> S3Backend b -> STM (Maybe b)
lookupDelay key backend = do
    state <- SMap.lookup key (s3BackendState backend)
    pure $ uploadProgressTimeout <$> state

-- Delete all objects with a prefix matching this backend.  Leave the
-- bucket alone in case there are other objects unrelated to this bucket
-- in it.
cleanupS3 :: S3Backend delay -> IO ()
cleanupS3 (S3Backend{s3BackendEnv, s3BackendBucket, s3BackendPrefix}) = runResourceT $ do
    resp <- AWS.send s3BackendEnv (S3.listObjects_prefix ?~ s3BackendPrefix $ S3.newListObjects s3BackendBucket)
    let objectKeys = catMaybes . traverse (S3.newObjectIdentifier . (^. object_key) <$>) $ resp ^. listObjectsResponse_contents

    unless (null objectKeys) . void $
        AWS.send s3BackendEnv (S3.newDeleteObjects s3BackendBucket (delete_objects .~ objectKeys $ S3.newDelete))

s3Backend :: IO (S3Backend FakeDelay)
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
