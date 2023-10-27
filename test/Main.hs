{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (
    guard,
    unless,
    void,
    when,
 )
import Data.Composition ((.:))
import qualified Data.Map as Map
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
    context,
    describe,
    hspec,
    it,
    parallel,
    shouldBe,
    shouldReturn,
    shouldThrow,
 )

import TahoeLAFS.Storage.API (
    AllocateBuckets (AllocateBuckets),
    AllocationResult (AllocationResult, allocated, alreadyHave),
    CBORSet (CBORSet),
    LeaseSecret (Upload),
    Offset,
    ReadResult,
    ReadTestWriteResult (readData, success),
    ReadTestWriteVectors (ReadTestWriteVectors, readVector, testWriteVectors),
    ReadVector (ReadVector),
    ShareData,
    ShareNumber (..),
    Size,
    SlotSecrets (SlotSecrets, leaseCancel, leaseRenew, writeEnabler),
    StorageIndex,
    TestOperator (Eq),
    TestVector (TestVector),
    TestWriteVectors (TestWriteVectors, newLength, test, write),
    Version (parameters),
    Version1Parameters (maximumImmutableShareSize),
    WriteVector (WriteVector, shareData, writeOffset),
    toInteger,
 )
import Test.QuickCheck (
    Arbitrary (arbitrary),
    Gen,
    NonNegative (getNonNegative),
    Positive (Positive),
    Property,
    chooseInteger,
    forAll,
    listOf1,
    oneof,
    shuffle,
    sublistOf,
    withMaxSuccess,
 )
import Test.QuickCheck.Monadic (
    monadicIO,
    run,
 )

import qualified Data.ByteString as B

import TahoeLAFS.Storage.Backend (
    Backend (
        abortImmutableUpload,
        createImmutableStorageIndex,
        getImmutableShareNumbers,
        getMutableShareNumbers,
        readImmutableShare,
        readMutableShare,
        readvAndTestvAndWritev,
        version,
        writeImmutableShare
    ),
    WriteImmutableError (
        ImmutableShareAlreadyWritten,
        IncorrectUploadSecret,
        MissingUploadSecret,
        ShareNotAllocated
    ),
    writeMutableShare,
 )

import qualified Amazonka as AWS
import qualified Amazonka.S3 as S3
import Tahoe.Storage.Backend.S3 (
    BackendError (MaximumShareSizeExceeded),
    S3Backend (S3Backend, s3BackendBucket, s3BackendEnv, s3BackendPrefix),
    applyWriteVectors,
    newS3Backend,
 )

-- We also get the Arbitrary ShareNumber instance from here.
import Lib (
    genStorageIndex,
 )

import Amazonka (runResourceT)
import Amazonka.S3.Lens (delete_objects, listObjectsResponse_contents, object_key)
import qualified Amazonka.S3.Lens as S3
import Control.Exception (Exception, throw)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (catMaybes)
import Network.HTTP.Types (ByteRange (..))
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
    storageSpec s3Backend

{- | Define the maximum number of times some "simple" properties will be
 checked.  These are properties where the expectation is that the cardinality
 of the set of paths through the implementation is very small so the cost of
 checking hundreds of different inputs is not worth the benefit.
-}
few :: Int
few = 5

-- The specification for a storage backend.
storageSpec :: (Backend b, Mess b) => IO b -> Spec
storageSpec makeBackend = do
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

    context "v1" $ do
        context "immutable" $ do
            describe "allocate a storage index" $ do
                it "rejects allocations above the immutable share size limit" $
                    withMaxSuccess few $
                        forAll genStorageIndex $ \storageIndex (ShareNumbers shareNums) secret (Positive extra) -> do
                            withBackend makeBackend $ \backend -> do
                                limit <- maximumImmutableShareSize . parameters <$> version backend
                                createImmutableStorageIndex backend storageIndex (Just [Upload secret]) (AllocateBuckets shareNums (limit + extra))
                                    `shouldThrow` (== MaximumShareSizeExceeded limit (limit + extra))

                it "accounts for all allocated share numbers" $
                    forAll genStorageIndex (alreadyHavePlusAllocatedImm makeBackend)

            context "write a share" $ do
                it "disallows writing an unallocated share" $
                    withMaxSuccess few $
                        forAll genStorageIndex $ \storageIndex shareNum secret shareData ->
                            withBackend makeBackend $ \backend -> do
                                writeImmutableShare backend storageIndex shareNum (Just [Upload secret]) shareData Nothing
                                    `shouldThrow` (== ShareNotAllocated)

                it "disallows writes without an upload secret" $
                    withMaxSuccess few $
                        forAll genStorageIndex $ \storageIndex (ShareNumbers shareNums@(shareNum : _)) secret ->
                            withBackend makeBackend $ \backend -> do
                                void $ createImmutableStorageIndex backend storageIndex (Just [Upload secret]) (AllocateBuckets shareNums 100)
                                writeImmutableShare backend storageIndex shareNum Nothing "fooooo" Nothing `shouldThrow` (== MissingUploadSecret)

                it "disallows writes without a matching upload secret" $
                    withMaxSuccess few $
                        forAll genStorageIndex $ \storageIndex (ShareNumbers shareNums@(shareNum : _)) secret ->
                            withBackend makeBackend $ \backend -> do
                                void $ createImmutableStorageIndex backend storageIndex (Just [Upload secret]) (AllocateBuckets shareNums 100)
                                -- Supply the wrong secret as an upload secret and the
                                -- right secret marked for some other use - this
                                -- should still fail.
                                writeImmutableShare backend storageIndex shareNum (Just [Upload $ "x" <> secret]) "fooooo" Nothing `shouldThrow` (== IncorrectUploadSecret)

                it "disallows upload completion after a successful abort" $
                    withMaxSuccess few $
                        forAll genStorageIndex $ \storageIndex shareNum secret shareData size ->
                            withBackend makeBackend $ \backend -> do
                                void $ createImmutableStorageIndex backend storageIndex (Just [Upload secret]) (AllocateBuckets [shareNum] size)
                                abortImmutableUpload backend storageIndex shareNum (Just [Upload secret])
                                writeImmutableShare backend storageIndex shareNum (Just [Upload secret]) shareData Nothing
                                    `shouldThrow` (== ShareNotAllocated)

                it "returns the share numbers that were written" $
                    forAll genStorageIndex (immutableWriteAndEnumerateShares makeBackend)

                it "returns the written data when requested" $
                    forAll genStorageIndex (immutableWriteAndReadShare makeBackend)

                it "cannot be written more than once" $
                    withMaxSuccess few $
                        forAll genStorageIndex (immutableWriteAndRewriteShare makeBackend)

                it "disallows aborts without an upload secret" $
                    withMaxSuccess few $
                        forAll genStorageIndex $ \storageIndex shareNum ->
                            withBackend makeBackend $ \backend -> do
                                abortImmutableUpload backend storageIndex shareNum Nothing `shouldThrow` (== MissingUploadSecret)

            describe "aborting uploads" $ do
                it "disallows aborts without a matching upload secret" $
                    withMaxSuccess few $
                        withBackend makeBackend $ \backend -> do
                            AllocationResult [] [ShareNumber 0] <- createImmutableStorageIndex backend "storageindex" (Just [Upload "thesecret"]) (AllocateBuckets [ShareNumber 0] 100)
                            abortImmutableUpload backend "storageindex" (ShareNumber 0) (Just [Upload "wrongsecret"]) `shouldThrow` (== IncorrectUploadSecret)

                it "allows aborts with a matching upload secret" $
                    withBackend makeBackend $ \backend -> do
                        AllocationResult [] [ShareNumber 0] <- createImmutableStorageIndex backend "storageindex" (Just [Upload "thesecret"]) (AllocateBuckets [ShareNumber 0] 100)
                        abortImmutableUpload backend "storageindex" (ShareNumber 0) (Just [Upload "thesecret"])

        context "mutable" $ do
            describe "write a share" $ do
                it "returns the share numbers that were written" $
                    forAll genStorageIndex (mutableWriteAndEnumerateShares makeBackend)

                it "returns the written data when requested" $
                    forAll genStorageIndex (mutableWriteAndReadShare makeBackend)

                it "accepts writes for which the test condition succeeds" $
                    withBackend makeBackend $ \backend -> do
                        writeMutable backend "storageindex" (ShareNumber 0) [] [WriteVector 0 "abc"]
                        writeMutable backend "storageindex" (ShareNumber 0) [TestVector 0 3 Eq "abc"] [WriteVector 0 "xyz"]
                        readMutableShare backend "storageindex" (ShareNumber 0) Nothing `shouldReturn` "xyz"

                it "rejects writes for which the test condition fails" $
                    withBackend makeBackend $ \backend -> do
                        writeMutable backend "storageindex" (ShareNumber 0) [] [WriteVector 0 "abc"]
                        writeMutable backend "storageindex" (ShareNumber 0) [TestVector 0 3 Eq "abd"] [WriteVector 0 "xyz"]
                            `shouldThrow` (\WriteRefused{} -> True)
                        readMutableShare backend "storageindex" (ShareNumber 0) Nothing `shouldReturn` "abc"

                it "retrieves share data from before writes are applied" $ do
                    withBackend makeBackend $ \backend -> do
                        runReadTestWrite_ backend "storageindex" $ (emptyRTW `withWrite` ShareNumber 0) (WriteVector 0 "abc")
                        runReadTestWrite backend "storageindex" ((emptyRTW `withRead` ReadVector 0 3 `withWrite` ShareNumber 0) (WriteVector 0 "xyz"))
                            `shouldReturn` Map.fromList [(ShareNumber 0, ["abc"])]
                        runReadTestWrite backend "storageindex" (emptyRTW `withRead` ReadVector 0 3)
                            `shouldReturn` Map.fromList [(ShareNumber 0, ["xyz"])]

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

alreadyHavePlusAllocatedImm ::
    (Backend b, Mess b) =>
    -- | The backend on which to operate
    IO b ->
    -- | The storage index to use
    StorageIndex ->
    -- | The first group of share numbers to allocate.
    ShareNumbers ->
    -- | The second, maybe overlapping, group of share numbers -- to allocate.
    ShareNumbers ->
    -- | The size of each share
    Positive Size ->
    Property
alreadyHavePlusAllocatedImm
    makeBackend
    storageIndex
    (ShareNumbers firstGroup)
    (ShareNumbers secondGroup)
    (Positive size) = monadicIO . run . withBackend makeBackend $ \backend -> do
        -- Allocate some shares.  The property should hold.
        allocate backend (AllocateBuckets firstGroup size) >>= theInvariant firstGroup
        -- Allocate some other shares on the same storage index.  The property should hold.
        allocate backend (AllocateBuckets secondGroup size) >>= theInvariant secondGroup
      where
        -- Do some allocation.
        allocate b = createImmutableStorageIndex b storageIndex (Just [Upload "hello world"])

        -- Check the property.
        theInvariant allocated' result = do
            when (alreadyHave result ++ allocated result /= allocated') $
                fail
                    ( show (alreadyHave result)
                        ++ " ++ "
                        ++ show (allocated result)
                        ++ " /= "
                        ++ show allocated'
                    )

-- The share numbers of immutable share data written to the shares of a given
-- storage index can be retrieved.
immutableWriteAndEnumerateShares ::
    (Backend b, Mess b) =>
    IO b ->
    StorageIndex ->
    ShareNumbers ->
    B.ByteString ->
    Property
immutableWriteAndEnumerateShares makeBackend storageIndex (ShareNumbers shareNumbers) shareSeed = monadicIO . run . withBackend makeBackend $ \backend -> do
    void $ createImmutableStorageIndex backend storageIndex uploadSecret allocate
    writeShares (\sn -> writeImmutableShare backend storageIndex sn uploadSecret) (zip shareNumbers permutedShares)
    readShareNumbers <- getImmutableShareNumbers backend storageIndex
    when (readShareNumbers /= (CBORSet . Set.fromList $ shareNumbers)) $
        fail (show readShareNumbers ++ " /= " ++ show shareNumbers)
  where
    permutedShares = Prelude.map (permuteShare shareSeed) shareNumbers
    size = fromIntegral (B.length shareSeed)
    allocate = AllocateBuckets shareNumbers size
    uploadSecret = Just [Upload "hello"]

-- Immutable share data written to the shares of a given storage index cannot
-- be rewritten by a subsequent writeImmutableShare operation.
immutableWriteAndRewriteShare ::
    (Backend b, Mess b) =>
    IO b ->
    StorageIndex ->
    ShareNumbers ->
    B.ByteString ->
    Property
immutableWriteAndRewriteShare makeBackend storageIndex (ShareNumbers shareNumbers) shareSeed = monadicIO . run . withBackend makeBackend $ \backend -> do
    void $ createImmutableStorageIndex backend storageIndex uploadSecret allocate
    let write = writeImmutableShare backend storageIndex aShareNumber uploadSecret aShare Nothing
    write
    write `shouldThrow` (== ImmutableShareAlreadyWritten)
  where
    size = fromIntegral (B.length shareSeed)
    allocate = AllocateBuckets shareNumbers size
    aShareNumber = head shareNumbers
    aShare = permuteShare shareSeed aShareNumber
    uploadSecret = Just [Upload "hello"]

-- Immutable share data written to the shares of a given storage index can be
-- retrieved verbatim and associated with the same share numbers as were
-- specified during writing.
immutableWriteAndReadShare ::
    (Backend b, Mess b) =>
    IO b ->
    StorageIndex ->
    ShareNumbers ->
    B.ByteString ->
    Property
immutableWriteAndReadShare makeBackend storageIndex (ShareNumbers shareNumbers) shareSeed = monadicIO . run . withBackend makeBackend $ \backend -> do
    void $ createImmutableStorageIndex backend storageIndex uploadSecret allocate
    writeShares (\sn -> writeImmutableShare backend storageIndex sn uploadSecret) (zip shareNumbers permutedShares)
    readShares' <- mapM (\sn -> readImmutableShare backend storageIndex sn Nothing) shareNumbers
    when (permutedShares /= readShares') $
        fail (show permutedShares ++ " /= " ++ show readShares')
  where
    permutedShares = Prelude.map (permuteShare shareSeed) shareNumbers
    size = fromIntegral (B.length shareSeed)
    allocate = AllocateBuckets shareNumbers size
    uploadSecret = Just [Upload "hello"]

-- The share numbers of mutable share data written to the shares of a given
-- storage index can be retrieved.
mutableWriteAndEnumerateShares ::
    (Backend b, Mess b) =>
    IO b ->
    StorageIndex ->
    ShareNumbers ->
    B.ByteString ->
    Property
mutableWriteAndEnumerateShares makeBackend storageIndex (ShareNumbers shareNumbers) shareSeed = monadicIO . run . withBackend makeBackend $ \backend -> do
    writeShares (writeMutableShare backend nullSecrets storageIndex) (zip shareNumbers permutedShares)
    (CBORSet readShareNumbers) <- getMutableShareNumbers backend storageIndex
    when (readShareNumbers /= Set.fromList shareNumbers) $
        fail (show readShareNumbers ++ " /= " ++ show shareNumbers)
  where
    permutedShares = Prelude.map (permuteShare shareSeed) shareNumbers

data MutableWriteExample = MutableWriteExample
    { mweShareNumber :: ShareNumber
    , mweShareData :: [B.ByteString]
    , mweReadRange :: Maybe [ByteRange]
    }
    deriving (Show)

instance Arbitrary MutableWriteExample where
    arbitrary = do
        mweShareNumber <- arbitrary
        mweShareData <- listOf1 (B.pack <$> listOf1 arbitrary)
        mweReadRange <- byteRanges (fromIntegral . sum . fmap B.length $ mweShareData)
        pure MutableWriteExample{..}

-- | ByteRange type lets us use illegal values like -1 but then things go poorly
byteRanges :: Integer -> Gen (Maybe [ByteRange])
byteRanges dataSize =
    oneof
        [ -- A request for all the bytes.
          pure Nothing
        , -- A request for bytes starting from and including some zero-indexed
          -- position and running to the end of the data.
          Just . (: []) . ByteRangeFrom <$> chooseInteger (0, dataSize - 1)
        , -- A request for bytes starting from and including some zero-indexed
          -- position and running to and including another zero-indexed
          -- position.
          Just . (: []) .: fromTo <$> chooseInteger (0, dataSize - 1) <*> chooseInteger (0, dataSize)
        , -- A request for a certain number of bytes of suffix.
          Just . (: []) . ByteRangeSuffix <$> chooseInteger (1, dataSize + 1)
        ]
  where
    fromTo a b = ByteRangeFromTo a (a + b)

{- | After an arbitrary number of separate writes complete to construct the
 entire share, any range of the share's bytes can be read.
-}
mutableWriteAndReadShare ::
    (Backend b, Mess b) =>
    IO b ->
    StorageIndex ->
    MutableWriteExample ->
    Property
mutableWriteAndReadShare makeBackend storageIndex MutableWriteExample{..} = monadicIO . run . withBackend makeBackend $ \backend -> do
    mapM_ (uncurry $ writeMutableShareChunk backend nullSecrets storageIndex mweShareNumber) (zip mweShareData (offsetsFor mweShareData))
    readMutableShare backend storageIndex mweShareNumber mweReadRange `shouldReturn` shareRange
  where
    offsetsFor ranges = scanl (+) 0 $ map (fromIntegral . B.length) ranges

    shareRange :: B.ByteString
    shareRange = case mweReadRange of
        Nothing -> B.concat mweShareData
        Just ranges -> B.concat $ readRange (B.concat mweShareData) <$> ranges

    readRange shareData (ByteRangeFrom start) = B.drop (fromIntegral start) shareData
    readRange shareData (ByteRangeFromTo start end) = B.take (fromIntegral $ (end - start) + 1) . B.drop (fromIntegral start) $ shareData
    readRange shareData (ByteRangeSuffix len) = B.drop (B.length shareData - fromIntegral len) shareData

permuteShare :: B.ByteString -> ShareNumber -> B.ByteString
permuteShare seed number =
    B.map xor' seed
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

nullSecrets :: SlotSecrets
nullSecrets =
    SlotSecrets
        { writeEnabler = ""
        , leaseRenew = ""
        , leaseCancel = ""
        }

data WriteRefused = WriteRefused deriving (Show, Eq)
instance Exception WriteRefused

runReadTestWrite :: Backend b => b -> StorageIndex -> ReadTestWriteVectors -> IO ReadResult
runReadTestWrite backend storageIndex rtw = do
    result <- readvAndTestvAndWritev backend storageIndex rtw
    if success result then pure $ readData result else throw WriteRefused

runReadTestWrite_ :: Backend b => b -> StorageIndex -> ReadTestWriteVectors -> IO ()
runReadTestWrite_ backend storageIndex rtw = void $ runReadTestWrite backend storageIndex rtw

writeMutableShareChunk :: Backend b => b -> SlotSecrets -> StorageIndex -> ShareNumber -> ShareData -> Offset -> IO ()
writeMutableShareChunk b _secrets storageIndex shareNum shareData offset = do
    void $ runReadTestWrite b storageIndex rtw
  where
    rtw = (emptyRTW `withWrite` shareNum) WriteVector{writeOffset = offset, shareData = shareData}

writeMutable ::
    Backend b =>
    b ->
    StorageIndex ->
    ShareNumber ->
    [TestVector] ->
    [WriteVector] ->
    IO ()
writeMutable backend storageIndex shareNum testv writev = do
    void $ runReadTestWrite backend storageIndex rtw
  where
    rtw = ((emptyRTW `withTests` shareNum) testv `withWrites` shareNum) writev

emptyRTW :: ReadTestWriteVectors
emptyRTW = ReadTestWriteVectors{testWriteVectors = mempty, readVector = []}

withRead :: ReadTestWriteVectors -> ReadVector -> ReadTestWriteVectors
withRead r@ReadTestWriteVectors{..} newRead = r{readVector = newRead : readVector}

withReads :: ReadTestWriteVectors -> [ReadVector] -> ReadTestWriteVectors
withReads rtw [] = rtw
withReads rtw@ReadTestWriteVectors{readVector} vs = rtw{readVector = vs <> readVector}

withMany :: (t1 -> t2 -> t3 -> t1) -> t1 -> t2 -> [t3] -> t1
withMany _ rtw _ [] = rtw
withMany f rtw shareNum (x : xs) = withMany f (f rtw shareNum x) shareNum xs

withTest :: ReadTestWriteVectors -> ShareNumber -> TestVector -> ReadTestWriteVectors
withTest r@ReadTestWriteVectors{..} shareNum newTest = r{testWriteVectors = Map.alter f shareNum testWriteVectors}
  where
    f Nothing = Just TestWriteVectors{test = [newTest], write = [], newLength = Nothing}
    f (Just t) = Just t{test = newTest : test t}

withTests :: ReadTestWriteVectors -> ShareNumber -> [TestVector] -> ReadTestWriteVectors
withTests = withMany withTest

withWrite :: ReadTestWriteVectors -> ShareNumber -> WriteVector -> ReadTestWriteVectors
withWrite r@ReadTestWriteVectors{..} shareNum newWrite = r{testWriteVectors = Map.alter f shareNum testWriteVectors}
  where
    f Nothing = Just TestWriteVectors{test = [], write = [newWrite], newLength = Nothing}
    f (Just t) = Just t{write = newWrite : write t}

withWrites :: ReadTestWriteVectors -> ShareNumber -> [WriteVector] -> ReadTestWriteVectors
withWrites = withMany withWrite

withLength :: ReadTestWriteVectors -> ShareNumber -> Integer -> ReadTestWriteVectors
withLength r@ReadTestWriteVectors{..} shareNum newLength = r{testWriteVectors = Map.alter f shareNum testWriteVectors}
  where
    f Nothing = Just TestWriteVectors{test = [], write = [], newLength = Just newLength}
    f (Just t) = Just t{newLength = Just newLength}
