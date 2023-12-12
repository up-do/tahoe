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
 )
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString as B
import qualified Data.FingerTree as FT
import Data.Foldable (Foldable (toList), fold, foldl')
import Data.List (scanl')
import Data.Maybe (catMaybes, isJust)
import qualified Data.Text as T
import Data.Word (Word8)
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
    Size,
    StorageIndex,
    UploadSecret (UploadSecret),
    WriteImmutableError (ShareNotAllocated),
    WriteVector (WriteVector),
 )
import qualified Tahoe.Storage.Backend.Internal.BufferedUploadTree as UT
import Tahoe.Storage.Backend.S3 (
    AWS,
    Minio,
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
    hspec,
    it,
    parallel,
    shouldBe,
    shouldReturn,
    shouldSatisfy,
    shouldThrow,
 )
import Test.QuickCheck (
    Arbitrary (arbitrary),
    NonEmptyList (..),
    forAll,
    label,
    shuffle,
    (.&&.),
    (.||.),
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
                        parts = sizedParts (zipWith UT.PartData intervals chunks)
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
                        parts = sizedParts (zipWith UT.PartData intervals chunks)
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
                        parts = sizedParts (zipWith UT.PartData intervals chunks)
                    forAll (shuffle parts) $ \parts' -> do
                        let manyInsertsTree = foldl' (flip UT.insert) emptyTree parts'
                            singleInsertTree = UT.insert (UT.PartData (fold intervals) (fold chunks) (sum sizes)) emptyTree

                        FT.measure (UT.uploadTree manyInsertsTree) === FT.measure (UT.uploadTree singleInsertTree)

            it "allows inserts in any order explicit" $ do
                let parts' =
                        sizedParts
                            [ UT.PartData (UT.Interval 1 3) "123"
                            , UT.PartData (UT.Interval 5 8) "5678"
                            , UT.PartData (UT.Interval 4 4) "4"
                            , UT.PartData (UT.Interval 10 10) "A"
                            , UT.PartData (UT.Interval 9 9) "9"
                            , UT.PartData (UT.Interval 0 0) "0"
                            ]
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
                        sizedParts
                            [ UT.PartData (UT.Interval 6 10) "6789A"
                            , UT.PartData (UT.Interval 0 5) "012345"
                            ]
                    tree1 = foldl' (flip UT.insert) emptyTree parts1
                    tree2 = foldl' (flip UT.insert) emptyTree (toList (UT.uploadTree tree1))
                tree1 === tree2

            it "has correct tree measure (simple, backwards)" $ do
                let parts' =
                        sizedParts
                            [ UT.PartData (UT.Interval 6 10) "6789A"
                            , UT.PartData (UT.Interval 0 5) "012345"
                            ]
                    tree = foldl' (flip UT.insert) emptyTree parts'
                tree === UT.insert (UT.PartData (UT.Interval 0 10) "0123456789A" 11) emptyTree
            -- FT.measure (UT.uploadTree tree) === UT.UploadTreeMeasure (UT.Interval 0 10) 11 1 False

            it "has correct tree measure (simple)" $ do
                let parts' =
                        sizedParts
                            [ UT.PartData (UT.Interval 0 5) "012345"
                            , UT.PartData (UT.Interval 6 10) "6789A"
                            ]
                    tree = foldl' (flip UT.insert) emptyTree parts'
                FT.measure (UT.uploadTree tree) === UT.UploadTreeMeasure (UT.Interval 0 10) 11 1 False

            it "has correct tree measure" $ do
                let parts' =
                        sizedParts
                            [ UT.PartData (UT.Interval 1 3) "123"
                            , UT.PartData (UT.Interval 5 8) "5678"
                            , UT.PartData (UT.Interval 4 4) "4"
                            , UT.PartData (UT.Interval 10 10) "A"
                            , UT.PartData (UT.Interval 9 9) "9"
                            , UT.PartData (UT.Interval 0 0) "0"
                            ]
                    tree = foldl' (flip UT.insert) emptyTree parts'
                FT.measure (UT.uploadTree tree) === UT.UploadTreeMeasure (UT.Interval 0 10) 11 1 False

            it "get uploadable chunks explicit" $ do
                let parts' =
                        sizedParts
                            [ UT.PartData (UT.Interval 1 3) "123"
                            , UT.PartData (UT.Interval 5 8) "5678"
                            , UT.PartData (UT.Interval 4 4) "4"
                            , UT.PartData (UT.Interval 10 10) "A"
                            , UT.PartData (UT.Interval 9 9) "9"
                            , UT.PartData (UT.Interval 0 0) "0"
                            ]
                    tree = foldl' (flip UT.insert) emptyTree parts'
                    (uploadable, tree') = UT.findUploadableChunk tree 1
                uploadable
                    === Just (UT.UploadInfo (UT.PartNumber 1) "0123456789A")
                    .&&. tree'
                    === UT.UploadTree (FT.fromList [UT.PartUploading (UT.PartNumber 1) (UT.Interval 0 10)])

            it "an uploadable chunk with unusable bytes on the left can be found" $ do
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
                    (uploadable, tree') = UT.findUploadableChunk tree 1
                UT.uploadTree tree'
                    === FT.fromList
                        [ UT.PartData (UT.Interval 0 3) "0123" 22
                        , UT.PartData (UT.Interval 5 10) "56789A" 22
                        , UT.PartUploading (UT.PartNumber 2) (UT.Interval 11 21)
                        ]
                    .&&. (UT.uploadInfoBytes <$> uploadable)
                    === Just "BCDEFGHIJKL"

            it "an uploadable part can be found on the right hand side of a disconnected tree" $ do
                let parts' =
                        [ UT.PartData (UT.Interval 0 1) "01" 22
                        , UT.PartData (UT.Interval 11 21) "BCDEFGHIJKL" 22
                        ]
                    tree = foldl' (flip UT.insert) emptyTree parts'
                    (uploadable, tree') = UT.findUploadableChunk tree 1
                uploadable
                    === Just (UT.UploadInfo (UT.PartNumber 2) "BCDEFGHIJKL")
                    .&&. UT.uploadTree tree'
                    === FT.fromList
                        [ UT.PartData (UT.Interval 0 1) "01" 22
                        , UT.PartUploading (UT.PartNumber 2) (UT.Interval 11 21)
                        ]

            it "insert finds uploadable data regardless of gaps and insertion order" $
                forAll arbitrary $ \sizeIncrements -> do
                    let sizes = scanr (+) 1 (fromIntegral @Word8 @Int . getPositive <$> sizeIncrements)
                        chunks = zipWith B.replicate sizes (cycle [97 .. 97 + 26])
                        -- offsets, but with a 1-byte gap in each
                        offsets :: [Integer]
                        offsets = scanl' (\a b -> a + b + 1) 0 (fromIntegral <$> sizes)
                        intervals = zipWith (\o chunk -> UT.Interval o (o + fromIntegral (B.length chunk - 1))) offsets chunks
                        parts = sizedParts (zipWith UT.PartData intervals chunks)

                        UT.PartSize partSize = UT.computePartSize @FakeBackend (UT.totalShareSize . head $ parts)

                        -- There might be more than one chunk with the same
                        -- number of parts, and those chunks might have more
                        -- parts than all the other chunks.  In this case, we
                        -- don't know which one we will find.  That's okay.
                        -- Just make sure we find one of them.
                        biggestPartCount = maximum $ UT.uploadableParts . FT.measure <$> parts

                    forAll (shuffle parts) $ \parts' ->
                        let tree = foldl' (flip UT.insert) emptyTree parts'
                            (Just found, _) = UT.findUploadableChunk tree biggestPartCount

                            -- Since we made none of the pieces contiguous, we
                            -- should get the same number out as we put in.
                            nodeCountMatches =
                                length (toList (UT.uploadTree tree)) === length chunks

                            -- If no piece contained at least one complete
                            -- part then it is expected that we cannot find
                            -- any uploadable chunks.
                            expectNoResult = biggestPartCount === 0

                            -- Otherwise, the chunk we found should have the
                            -- same bytes as we put in.  Since no pieces are
                            -- contiguous we can find the bytes that come out
                            -- inside one just one of those pieces.  We may
                            -- have to strip off some prefix and suffix for
                            -- the comparison though, depending on whether the
                            -- piece overruns part boundaries or not.
                            foundOffset = intervalForInfo partSize found
                            (_, right) =
                                FT.split
                                    ((\i -> UT.intervalLow foundOffset >= UT.intervalLow i) . UT.coveringInterval)
                                    (UT.uploadTree tree)
                            foundPiece FT.:< _ = FT.viewl right
                            uploadBytesConsistent =
                                UT.uploadInfoBytes found === partToBytes partSize foundPiece
                            -- As an extra precaution, check this very simple
                            -- property of the length of the bytes to upload.
                            -- I am quite sure it holds and it may catch bugs
                            -- in the more complex uploadBytesConsistent
                            -- property (it caught one already).
                            actualBytesCountConsistent =
                                B.length (UT.uploadInfoBytes found) `mod` fromIntegral partSize === 0
                         in nodeCountMatches .&&. (expectNoResult .||. actualBytesCountConsistent .&&. uploadBytesConsistent)

    context "backend" $ do
        describe "immutable uploads" $ do
            it "timeout when progress is not being made" $ do
                let storageIndex = "abcd"
                    secrets = Just [Upload (UploadSecret "hello")]
                    shareNums = ShareNumber <$> [1, 2, 3]
                    allocate = AllocateBuckets shareNums 123

                backend <- s3Backend @Minio
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
                backend <- s3Backend @Minio

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
                backend <- s3Backend @Minio

                -- Allocate some stuff
                createImmutableStorageIndex backend storageIndex secrets allocate
                    `shouldReturn` AllocationResult [] shareNums

                -- Allow less than the timeout to pass
                atomically $ s3TimePasses (immutableUploadProgressTimeout `div` 3 * 2) (storageIndex, ShareNumber 1) backend

                -- Then make some progress
                writeImmutableShare backend storageIndex (ShareNumber 1) secrets "Hello world" (Just [ByteRangeFromTo 0 10])

                -- Then allow more time to pass such that the total elapsed
                -- time exceeds the progress timeout.
                atomically $ s3TimePasses (immutableUploadProgressTimeout `div` 3 * 2) (storageIndex, ShareNumber 1) backend

                -- Then make some more progress.  This is allowed because the
                -- full progress timeout never elapsed without some progress
                -- being made.
                writeImmutableShare backend storageIndex (ShareNumber 1) secrets "Goodbye world" (Just [ByteRangeFromTo 11 23])

        describe "S3Backend" $ makeStorageSpec (s3Backend @Minio) cleanupS3

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

s3Backend :: IO (S3Backend b FakeDelay)
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

data FakeBackend

instance UT.IsBackend FakeBackend where
    minPartSize = UT.PartSize 11
    computePartSize _ = UT.PartSize 11

emptyTree :: UT.UploadTree FakeBackend ()
emptyTree = mempty

sizedParts :: [Size -> UT.Part a b] -> [UT.Part a b]
sizedParts ps = result
  where
    result = [p totalSize | p <- ps]
    totalSize = UT.intervalHigh (foldMap UT.getInterval result) + 1

{- | Get the data interval covered by a certain UploadInfo, given a certain
 part size.
-}
intervalForInfo :: Size -> UT.UploadInfo -> UT.Interval
intervalForInfo partSize UT.UploadInfo{uploadInfoPartNumber = UT.PartNumber partNum, uploadInfoBytes} = UT.Interval (fromIntegral l) (fromIntegral h)
  where
    l = (partNum - 1) * fromIntegral partSize
    h = l + fromIntegral (B.length uploadInfoBytes) - 1

{- | Find the contiguous bytes that belong to the given PartData and
 consistute full "parts" for a given part size.

 This parallels similar calculations in findUploadableChunk but tries to do so
 using different calculations to avoid duplicating any bugs there.
-}
partToBytes :: Size -> UT.Part a b -> B.ByteString
partToBytes partSize UT.PartData{getInterval, getShareData} =
    case B.length chunkBytes `mod` fromIntegral partSize of
        0 -> chunkBytes
        n -> error $ "partToBytes off by " <> show n <> " bytes"
  where
    partBoundaries = [0, partSize ..]

    -- The first part boundary that is >= the lower part boundary of
    -- getShareData.  This is exactly the lower bound of the data we want to
    -- return.
    boundLow = head $ dropWhile (< UT.intervalLow getInterval) partBoundaries

    -- The number of bytes we're going to discard from the left as belonging
    -- to an earlier incomplete part.
    prefixLength = boundLow - UT.intervalLow getInterval

    -- We want the largest number of complete parts.  We can determine that
    -- from the amount of data (less the bytes we're going to drop from the
    -- front) and the part size.
    dataLength = (UT.intervalSize getInterval - prefixLength) `div` partSize * partSize

    chunkBytes =
        -- Drop the prefix, if there is any.
        B.drop (fromIntegral prefixLength)
            -- Take the amount of data we want, discarding the suffix.
            . B.take (fromIntegral dataLength)
            $ getShareData
partToBytes _ _ = error "Can only convert PartData to bytes"
