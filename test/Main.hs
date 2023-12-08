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
import Control.Concurrent.STM (STM, atomically)
import qualified Control.Concurrent.STM.Map as SMap
import Control.Lens (view, (.~), (?~), (^.), _Just)
import Control.Monad (
    guard,
    unless,
    void,
    (<=<),
 )
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString as B
import qualified Data.FingerTree as FT
import Data.Foldable (Foldable (toList), fold, foldl')
import Data.List (scanl')
import Data.Maybe (catMaybes, isJust)
import qualified Data.Text as T
import Data.Word (Word8)
import Debug.Trace (trace)
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
import qualified Tahoe.Storage.Backend.Internal.BufferedUploadTree as UT
import Tahoe.Storage.Backend.S3 (
    AWS (..),
    S3Backend (..),
    UploadState (uploadProgressTimeout),
    applyWriteVectors,
    immutableUploadProgressTimeout,
    newS3Backend,
 )
import Tahoe.Storage.Testing.Spec (
    SomeShareData (..),
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
    runIO,
    shouldBe,
    shouldReturn,
    shouldSatisfy,
    shouldThrow,
 )
import Test.QuickCheck (
    Arbitrary (arbitrary),
    NonEmptyList (..),
    forAll,
    shuffle,
    (.&&.),
    (===),
 )
import Test.QuickCheck.Modifiers (Positive (getPositive))

main :: IO ()
main = hspec . parallel . describe "S3" $ spec

spec :: Spec
spec = do
    context "utilities" $ do
        describe "applyWriteVectors" $ do
            it "replaces existing bytes with new bytes" $
                applyWriteVectors "abc" [WriteVector 0 "x", WriteVector 1 "y", WriteVector 2 "z"] `shouldBe` "xyz"
            it "can extend the length of the result" $
                applyWriteVectors "abc" [WriteVector 3 "xyz"] `shouldBe` "abcxyz"
            it "fills gaps with NULs" $
                applyWriteVectors "abc" [WriteVector 5 "xyz"] `shouldBe` "abc\0\0xyz"
            it "applies writes left-to-right" $
                applyWriteVectors "abc" [WriteVector 0 "x", WriteVector 0 "y"] `shouldBe` "ybc"
            it "accepts partial overlaps" $
                applyWriteVectors "abc" [WriteVector 0 "xy", WriteVector 1 "zw"] `shouldBe` "xzw"

        describe "UploadTree" $ do
            it "allows inserts in any order" $
                forAll arbitrary $ \(NonEmpty shares) -> do
                    let chunks = getShareData <$> shares
                        sizes = fromIntegral . B.length <$> chunks
                        offsets = scanl' (+) 0 sizes
                        endpoints = subtract 1 <$> drop 1 offsets
                        intervals = zipWith UT.Interval offsets endpoints
                        parts = zipWith (\i c -> UT.PartData i c (sum sizes)) intervals chunks
                    forAll (shuffle parts) $ \parts' -> do
                        let tree = foldl' (flip UT.insert) emptyTree parts'
                        B.concat (UT.getShareData <$> toList (UT.uploadTree tree)) === B.concat chunks

            it "contiguous data is merged" $
                forAll arbitrary $ \(NonEmpty shares) -> do
                    let chunks = getShareData <$> shares
                        sizes = fromIntegral . B.length <$> chunks
                        offsets = scanl' (+) 0 sizes
                        endpoints = subtract 1 <$> drop 1 offsets
                        intervals = zipWith UT.Interval offsets endpoints
                        parts = zipWith (\i c -> UT.PartData i c (sum sizes)) intervals chunks
                    forAll (shuffle parts) $ \parts' -> do
                        let tree = foldl' (flip UT.insert) emptyTree parts'
                        length (toList (UT.uploadTree tree)) === 1

            it "has the same measurement if it has the same nodes" $
                forAll arbitrary $ \(NonEmpty shares) -> do
                    let chunks = getShareData <$> shares
                        sizes = fromIntegral . B.length <$> chunks
                        offsets = scanl' (+) 0 sizes
                        endpoints = subtract 1 <$> drop 1 offsets
                        intervals = zipWith UT.Interval offsets endpoints
                        parts = zipWith (\i c -> UT.PartData i c (sum sizes)) intervals chunks
                    forAll (shuffle parts) $ \parts' -> do
                        let manyInsertsTree = foldl' (flip UT.insert) emptyTree parts'
                            singleInsertTree = UT.insert (UT.PartData (fold intervals) (fold chunks) (sum sizes)) emptyTree

                        FT.measure (UT.uploadTree manyInsertsTree) === FT.measure (UT.uploadTree singleInsertTree)

            it "allows inserts in any order explicit" $ do
                let parts' =
                        [ makePart (UT.Interval 1 3) "123"
                        , makePart (UT.Interval 5 8) "5678"
                        , makePart (UT.Interval 4 4) "4"
                        , makePart (UT.Interval 10 10) "A"
                        , makePart (UT.Interval 9 9) "9"
                        , makePart (UT.Interval 0 0) "0"
                        ]
                    makePart i c = UT.PartData i c totalSize
                    totalSize = UT.intervalSize $ foldMap UT.getInterval parts'
                    tree = foldl' (flip UT.insert) emptyTree parts'
                B.concat (UT.getShareData <$> toList (UT.uploadTree tree)) === "0123456789A"

            it "measures full interval" $ do
                -- let p = (UT.PartData :: UT.Interval -> B.ByteString -> UT.Part AWS b) (UT.Interval 0 10) "0123456789A"
                let p = UT.PartData @FakeBackend (UT.Interval 0 10) "0123456789A" 11
                FT.measure p === UT.UploadTreeMeasure (UT.Interval 0 10) 11 1 False

            it "measures non-aligned interval, no chunks" $ do
                -- let p = (UT.PartData :: UT.Interval -> B.ByteString -> UT.Part AWS b) (UT.Interval 0 10) "0123456789A"
                let p = UT.PartData @FakeBackend (UT.Interval 1 12) "123456789ABC" 13
                FT.measure p === UT.UploadTreeMeasure (UT.Interval 1 12) 12 0 False

            it "measures non-aligned interval, have data" $ do
                -- let p = (UT.PartData :: UT.Interval -> B.ByteString -> UT.Part AWS b) (UT.Interval 0 10) "0123456789A"
                let p = UT.PartData @FakeBackend (UT.Interval 1 21) "123456789A0123456789A" 22
                FT.measure p === UT.UploadTreeMeasure (UT.Interval 1 21) 21 1 False

            it "merged tree equals already-merged tree" $ do
                let parts1 =
                        [ UT.PartData (UT.Interval 6 10) "6789A" 11
                        , UT.PartData (UT.Interval 0 5) "012345" 11
                        ]
                    tree1 = foldl' (flip UT.insert) emptyTree parts1
                    tree2 = foldl' (flip UT.insert) emptyTree (toList (UT.uploadTree tree1))
                tree1 === tree2

            it "has correct tree measure (simple, backwards)" $ do
                let parts' =
                        [ UT.PartData (UT.Interval 6 10) "6789A" 11
                        , UT.PartData (UT.Interval 0 5) "012345" 11
                        ]
                    tree = foldl' (flip UT.insert) emptyTree parts'
                tree === UT.insert (UT.PartData (UT.Interval 0 10) "0123456789A" 11) emptyTree
            -- FT.measure (UT.uploadTree tree) === UT.UploadTreeMeasure (UT.Interval 0 10) 11 1 False

            it "has correct tree measure (simple)" $ do
                let parts' =
                        [ UT.PartData (UT.Interval 0 5) "012345" 11
                        , UT.PartData (UT.Interval 6 10) "6789A" 11
                        ]
                    tree = foldl' (flip UT.insert) emptyTree parts'
                FT.measure (UT.uploadTree tree) === UT.UploadTreeMeasure (UT.Interval 0 10) 11 1 False

            it "has correct tree measure" $ do
                let parts' =
                        [ UT.PartData (UT.Interval 1 3) "123" 11
                        , UT.PartData (UT.Interval 5 8) "5678" 11
                        , UT.PartData (UT.Interval 4 4) "4" 11
                        , UT.PartData (UT.Interval 10 10) "A" 11
                        , UT.PartData (UT.Interval 9 9) "9" 11
                        , UT.PartData (UT.Interval 0 0) "0" 11
                        ]
                    tree = foldl' (flip UT.insert) emptyTree parts'
                FT.measure (UT.uploadTree tree) === UT.UploadTreeMeasure (UT.Interval 0 10) 11 1 False

            it "get uploadable chunks explicit" $ do
                let parts' =
                        [ UT.PartData (UT.Interval 1 3) "123" 11
                        , UT.PartData (UT.Interval 5 8) "5678" 11
                        , UT.PartData (UT.Interval 4 4) "4" 11
                        , UT.PartData (UT.Interval 10 10) "A" 11
                        , UT.PartData (UT.Interval 9 9) "9" 11
                        , UT.PartData (UT.Interval 0 0) "0" 11
                        ]
                    tree = foldl' (flip UT.insert) emptyTree parts'
                    (uploadable, tree') = UT.findUploadableChunk trivialAssigner tree 1
                uploadable
                    === Just (UT.UploadInfo (UT.PartNumber 1) "0123456789A")
                    .&&. tree'
                    === UT.UploadTree (FT.fromList [UT.PartUploading (UT.PartNumber 1) (UT.Interval 0 10)])

            it "get uploadable chunks explicit2" $ do
                let parts' =
                        [ UT.PartData (UT.Interval 1 3) "123" 22
                        , UT.PartData (UT.Interval 5 8) "5678" 22
                        , -- The last byte of the first part and 11 more bytes
                          -- to make a complete second part.
                          UT.PartData (UT.Interval 10 21) "ABCDEFGHIJKL" 22
                        , UT.PartData (UT.Interval 9 9) "9" 22
                        , UT.PartData (UT.Interval 0 0) "0" 22
                        ]
                    tree = foldl' (flip UT.insert) emptyTree parts'
                    (uploadable, tree') = UT.findUploadableChunk trivialAssigner tree 1
                UT.uploadTree tree'
                    === FT.fromList
                        [ UT.PartData (UT.Interval 0 3) "0123" 22
                        , UT.PartData (UT.Interval 5 10) "56789A" 22
                        , UT.PartUploading (UT.PartNumber 2) (UT.Interval 11 21)
                        ]
                    .&&. (UT.uploadInfoBytes <$> uploadable)
                    === Just "BCDEFGHIJK"

            it "wrong side tree" $ do
                let parts' =
                        [ UT.PartData (UT.Interval 0 1) "01" 9
                        , UT.PartData (UT.Interval 5 8) "5678" 9
                        ]
                    tree = foldl' (flip UT.insert) emptyTree parts'
                    (uploadable, tree') = UT.findUploadableChunk trivialAssigner tree 2
                uploadable === Just (UT.UploadInfo (UT.PartNumber 1) "01") -- .&&. tree' === FT.fromList [UT.PartUploading (UT.Interval 0 10)]
            it "uploadable data with gaps" $
                forAll arbitrary $ \sizeIncrements -> do
                    let sizes = scanr (+) 1 (fromIntegral @Word8 @Int . getPositive <$> sizeIncrements)
                        chunks = zipWith B.replicate sizes (cycle [97 .. 97 + 26])
                        -- offsets, but with a 1-byte gap in each
                        offsets :: [Integer]
                        offsets = scanl' (\a b -> a + b + 1) 0 (fromIntegral <$> sizes)
                        intervals = zipWith (\o chunk -> UT.Interval o (o + fromIntegral (B.length chunk - 1))) offsets chunks
                        parts = zipWith (\i c -> UT.PartData i c (fromIntegral $ sum sizes)) intervals chunks
                    forAll (shuffle parts) $ \parts' -> do
                        let tree = foldl' (flip UT.insert) emptyTree parts'
                        length (toList (UT.uploadTree tree))
                            === length chunks
                            .&&. fst (UT.findUploadableChunk trivialAssigner tree (fromIntegral $ head sizes))
                            === Just (toUploadInfo $ head parts)

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
s3TimePasses :: Int -> (StorageIndex, ShareNumber) -> S3Backend backend FakeDelay -> STM ()
s3TimePasses amount key S3Backend{..} = do
    maybeDelay <- fmap uploadProgressTimeout <$> SMap.lookup key s3BackendState
    case maybeDelay of
        Nothing -> error "no such thing to expire"
        Just tv -> timePasses amount tv

lookupDelay :: (StorageIndex, ShareNumber) -> S3Backend backend delay -> STM (Maybe delay)
lookupDelay key backend = do
    state <- SMap.lookup key (s3BackendState backend)
    pure $ uploadProgressTimeout <$> state

-- Delete all objects with a prefix matching this backend.  Leave the
-- bucket alone in case there are other objects unrelated to this bucket
-- in it.
cleanupS3 :: S3Backend backend delay -> IO ()
cleanupS3 (S3Backend{s3BackendEnv, s3BackendBucket, s3BackendPrefix}) = runResourceT $ do
    resp <- AWS.send s3BackendEnv (S3.listObjects_prefix ?~ s3BackendPrefix $ S3.newListObjects s3BackendBucket)
    let objectKeys = catMaybes . traverse (S3.newObjectIdentifier . (^. object_key) <$>) $ resp ^. listObjectsResponse_contents

    unless (null objectKeys) . void $
        AWS.send s3BackendEnv (S3.newDeleteObjects s3BackendBucket (delete_objects .~ objectKeys $ S3.newDelete))

s3Backend :: IO (S3Backend AWS FakeDelay)
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

{- | Assign a part number to the part at a given interval.  This assigner is
 extremely naive and uses the low bound of the interval plus one (to avoid
 illegally assigning 0).
-}
trivialAssigner = UT.PartNumber . succ . UT.intervalLow

toUploadInfo :: forall backend resp. UT.IsBackend backend => UT.Part backend resp -> UT.UploadInfo
toUploadInfo UT.PartData{getInterval, getShareData} = UT.UploadInfo (UT.PartNumber partNum) getShareData
  where
    UT.PartSize partSize = UT.computePartSize (UT.intervalSize getInterval) :: UT.PartSize backend
    partNum = UT.intervalLow getInterval `div` partSize
toUploadInfo _ = error "Cannot turn non-PartData into UploadInfo"

data FakeBackend

instance UT.IsBackend FakeBackend where
    minPartSize = UT.PartSize 11
    computePartSize totalSize = UT.PartSize 11

emptyTree :: UT.UploadTree FakeBackend ()
emptyTree = mempty
