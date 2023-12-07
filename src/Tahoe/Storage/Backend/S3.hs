{- | A Tahoe-LAFS Great Black Swamp storage backend which relies on an S3 server
 for data storage.

  Some notes about AWS limitations:

  - The maximum size of an S3 object created using the PutObject API is 5 GB.
  - The maximum size of an S3 object created using the CreateMultipartUpload / UploadPart is 5 TB.
  - The minimum size of a single UploadPart is 5 MB.
  - The maximum size of a single UploadPart is ???.
  - The maximum number of UploadParts associated with a CreateMultipartUpload is 10,000.
    - If you upload a 5 TB object using 10,000 parts each part is 500 MB.
  - UploadParts must be numbered using part numbers from 1 to 10,000 inclusive.
  - The part number of an UploadPart determines that part's payload's position in the resulting S3 object.
-}
module Tahoe.Storage.Backend.S3 where

import Amazonka (runResourceT)
import qualified Amazonka as AWS
import qualified Amazonka.S3 as S3
import qualified Amazonka.S3.Lens as S3
import Conduit (ResourceT, sinkList)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (concurrently, mapConcurrently, mapConcurrently_, race)
import Control.Concurrent.STM.Lifted (STM, atomically)
import qualified Control.Concurrent.STM.Map as SMap
import Control.Exception (Exception, SomeException, catch, throw, throwIO, try)
import Control.Lens (set, view, (.~), (?~), (^.))
import Control.Monad (void, when)
import Control.Monad.Catch (MonadCatch, onException)
import Data.Bifunctor (Bifunctor (first, second))
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as C8
import Data.Either (rights)
import qualified Data.FingerTree as FT
import Data.Foldable (Foldable (toList), fold, foldl', traverse_)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.IntervalMap.FingerTree as IMap
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Debug.Trace (trace)
import GHC.Conc (throwSTM)
import GHC.Stack (HasCallStack)
import Network.HTTP.Types (ByteRange (ByteRangeFrom, ByteRangeFromTo), Status (Status, statusCode))
import Network.HTTP.Types.Header (renderByteRange)
import Tahoe.Storage.Backend (
    AllocateBuckets (AllocateBuckets, allocatedSize, shareNumbers),
    AllocationResult (..),
    Backend (..),
    CBORSet (..),
    LeaseSecret (..),
    Offset,
    QueryRange,
    ReadTestWriteResult (..),
    ReadTestWriteVectors (..),
    ReadVector (..),
    ShareData,
    ShareNumber (..),
    Size,
    StorageIndex,
    TestOperator (Eq),
    TestVector (..),
    TestWriteVectors (..),
    UploadSecret,
    Version (Version),
    Version1Parameters (..),
    WriteEnablerSecret (..),
    WriteImmutableError (..),
    WriteMutableError (..),
    WriteVector (..),
 )
import Tahoe.Storage.Backend.Internal.BufferedUploadTree (IsBackend)
import qualified Tahoe.Storage.Backend.Internal.BufferedUploadTree as UT
import Tahoe.Storage.Backend.Internal.Delay (HasDelay (..), TimeoutOperation (Cancelled, Delayed))
import TahoeLAFS.Storage.Backend (
    withUploadSecret,
 )
import Text.Read (readMaybe)

-- Where's my Cow? -- Terry Pratchett

data AWS

instance UT.IsBackend AWS where
    minPartSize = UT.PartSize 5_000_000
    computePartSize totalSize = max UT.minPartSize (UT.PartSize $ totalSize `div` 10_000)

-- | This saves in-progress uploads so we can finish or abort
data UploadState backend delay where
    UploadState ::
        { -- | The total size of the data for this particular upload (ie, the
          -- share size).
          uploadStateSize :: Integer
        , uploadParts :: UT.UploadTree backend S3.UploadPartResponse
        , uploadResponse :: Maybe S3.CreateMultipartUploadResponse
        , uploadSecret :: UploadSecret
        , uploadProgressTimeout :: delay
        } ->
        UploadState backend delay

instance Show (UploadState backend delay) where
    show UploadState{..} = "<UploadState size=" <> show uploadStateSize <> ">"

{- | where's mah parts? Here they are!

 TODO This might leak space because normal deletes on SMap.Map don't reduce
 memory usage.  It might be nice to deal with this eventually.
-}
type AllocatedShares backend delay = SMap.Map (StorageIndex, ShareNumber) (UploadState backend delay)

-- | The top-level type of the S3 backend implementation.
data S3Backend backend delay where
    S3Backend ::
        { -- | Details about how to talk to the S3 server (for example, to
          -- establish connections and authorizer requests).
          s3BackendEnv :: AWS.Env
        , -- | The name of the S3 bucket in which this backend will store all
          -- data.
          s3BackendBucket :: S3.BucketName
        , -- | A prefix to apply to all S3 object keys operated on by this
          -- backend.
          s3BackendPrefix :: T.Text
        , -- | Internal state relating to immutable uploads that are in
          -- progress.
          s3BackendState :: AllocatedShares backend delay
        } ->
        S3Backend backend delay

internalAllocate :: UT.IsBackend backend => StorageIndex -> UploadSecret -> Integer -> ShareNumber -> delay -> AllocatedShares backend delay -> STM Bool
internalAllocate storageIndex secret allocatedSize shareNumber timeout curState = do
    SMap.member (storageIndex, shareNumber) curState >>= \case
        True -> pure False
        False -> do
            SMap.insert (storageIndex, shareNumber) newShareState curState
            pure True
  where
    newShareState =
        UploadState
            { uploadStateSize = allocatedSize
            , uploadParts = UT.UploadTree mempty
            , uploadResponse = Nothing
            , uploadSecret = secret
            , uploadProgressTimeout = timeout
            }

undoInternalAllocate :: StorageIndex -> ShareNumber -> AllocatedShares backend delay -> STM ()
undoInternalAllocate storageIndex shareNum = SMap.delete (storageIndex, shareNum)

internalAllocateComplete :: StorageIndex -> ShareNumber -> S3.CreateMultipartUploadResponse -> AllocatedShares backend delay -> STM ()
internalAllocateComplete storageIndex shareNum response curState = do
    SMap.lookup stateKey curState >>= \case
        Just state ->
            SMap.insert stateKey state{uploadResponse = Just response} curState
        Nothing ->
            error "Oh nooo contact Shae in building 42"
  where
    stateKey = (storageIndex, shareNum)

{- | If the identified share has been allocated and the given secrets match
 its upload secret then return new state including one new part upload for
 that share and that part upload's state.  Otherwise, throw an error.
-}
internalStartUpload ::
    UT.IsBackend backend =>
    -- | Secrets provided to authorize this upload.
    Maybe [LeaseSecret] ->
    -- | The storage index of the share the upload will be part of.
    StorageIndex ->
    -- | The number of the share the upload will be part of.
    ShareNumber ->
    -- | The position of some new bytes belonging to this share.
    Offset ->
    -- | The new bytes themselves.
    ShareData ->
    -- | The current total share state.
    AllocatedShares backend delay ->
    -- | The updated total share state and the state necessary to complete the
    -- upload to the S3 server.
    STM (Maybe PartUpload)
internalStartUpload secrets storageIndex shareNum offset shareData curState = do
    uploadState <- SMap.lookup stateKey curState
    let secret = uploadSecret <$> uploadState
    case secret of
        Nothing -> throw ShareNotAllocated
        Just secret'
            | validUploadSecret secret' secrets ->
                adjust' (first Just . startPartUpload offset shareData) stateKey curState
            | otherwise -> throw IncorrectUploadSecret
  where
    stateKey = (storageIndex, shareNum)

internalFinishUpload ::
    UT.IsBackend backend =>
    Integer ->
    C8.ByteString ->
    StorageIndex ->
    ShareNumber ->
    S3.UploadPartResponse ->
    AllocatedShares backend delay ->
    STM (Maybe (Bool, [S3.CompletedPart]))
internalFinishUpload partNum' shareData storageIndex shareNum response =
    adjust' (finishPartUpload (UT.PartNumber partNum') (fromIntegral $ B.length shareData) response) stateKey
  where
    stateKey = (storageIndex, shareNum)

infiniteRetry :: MonadCatch m => m b -> m b
infiniteRetry a = a `onException` infiniteRetry a

externalAllocate :: S3Backend backend delay -> StorageIndex -> ShareNumber -> ResourceT IO S3.CreateMultipartUploadResponse
externalAllocate S3Backend{..} storageIndex shareNum =
    -- XXX HOW TO HANDLE FAILURE? this AWS.send should really be AWS.sendEither !
    -- and then we handle errors that we can
    infiniteRetry $ AWS.send s3BackendEnv uploadCreate
  where
    objectKey = storageIndexShareNumberToObjectKey s3BackendPrefix storageIndex shareNum
    uploadCreate = S3.newCreateMultipartUpload s3BackendBucket objectKey

-- runResourceT $
--     do
--         -- switch to sendEither when more brain is available
--         -- XXX Check to see if we already have local state related to this upload
--         -- XXX Check to see if S3 already has state related to this upload
--         -- XXX Use thread-safe primitives so the server can be multithreaded
--         let objectKeys = storageIndexShareNumberToObjectKey s3BackendPrefix storageIndex <$> shareNumbers
--             uploadCreates = S3.newCreateMultipartUpload s3BackendBucket <$> objectKeys
--         -- XXX HOW TO HANDLE FAILURE? this AWS.send should really be AWS.sendEither !
--         -- and then we handle errors that we can
--         multipartUploadResponse <- mapM (fmap (flip (UploadState allocatedSize []) uploadSecret) . AWS.send s3BackendEnv) uploadCreates
--         let keys = zip (repeat storageIndex) shareNumbers :: [(StorageIndex, ShareNumber)]
--             newMap = Map.fromList $ zip keys multipartUploadResponse
--         -- don't send anything that's already there
--         liftIO $ modifyIORef' s3BackendState (newMap `Map.union`)
--         pure $ AllocationResult{alreadyHave = [], allocated = shareNumbers}

data PartUpload
    = -- | The associated CreateMultipartUpload has not yet completed.  XXX We
      -- should perhaps conceal this case?
      NotReady
    | -- | A write overlapped with some positions which were already written.
      Collision
    | -- | Some data has been accepted but it is not yet enough to be worth
      -- beginning a PartUpload.  We will hang on to it until more data is
      -- contributed.
      Buffering
    | -- | Some data should get uploaded now as a single part.
      StartUpload UT.PartNumber T.Text ShareData

{- | Add some more share data to the given state.  If there is enough
 contiguous data, also add another part upload and return the part number
 and upload id to use with it.
-}
startPartUpload :: UT.IsBackend backend => Offset -> ShareData -> UploadState backend delay -> (UploadState backend delay, PartUpload)
startPartUpload offset shareData u@UploadState{uploadStateSize, uploadParts, uploadResponse} =
    case view S3.createMultipartUploadResponse_uploadId <$> uploadResponse of
        -- If we don't have the upload response yet, we don't know the upload id
        -- so we can't actually handle this work.
        Nothing -> (u, NotReady)
        -- If we do, though, we can try to merge the new data into our idea
        -- about the state for this share.  That will produce a new state to
        -- return as well as, possibly, an action for the caller to take based
        -- on that new state.
        Just uploadId -> (state, action)
          where
            state = u{uploadParts = partsActioned}

            size = fromIntegral $ B.length shareData
            interval = UT.Interval (fromIntegral offset) (fromIntegral $ offset + size - 1)
            part = UT.PartData interval shareData uploadStateSize

            partsInserted = UT.insert part uploadParts
            (uploadable, partsActioned) = UT.findUploadableChunk (const (UT.PartNumber 1)) partsInserted 500

            action = case uploadable of
                Nothing -> Buffering
                Just (UT.UploadInfo partNum partData) -> StartUpload partNum uploadId partData

{- | Mark a part upload as finished and compute the new overall state of the
 multipart upload.
-}
finishPartUpload :: forall backend delay. UT.IsBackend backend => UT.PartNumber -> Integer -> S3.UploadPartResponse -> UploadState backend delay -> (Maybe (UploadState backend delay), (Bool, [S3.CompletedPart]))
finishPartUpload finishedPartNum finishedPartSize response u@UploadState{uploadParts, uploadStateSize} =
    ( if multipartFinished then Nothing else Just u{uploadParts = newUploadParts}
    ,
        ( multipartFinished
        , mapMaybe toCompleted . toList . UT.uploadTree $ newUploadParts
        )
    )
  where
    -- The multipart upload is finished every individual part has been
    -- uploaded and the interval covers all of the data.
    multipartFinished = UT.fullyUploaded m && UT.coveringInterval m == UT.Interval 0 (fromIntegral $ uploadStateSize - 1)
      where
        m = FT.measure (UT.uploadTree newUploadParts)

    -- The part which we just finished uploading changes from a
    -- `PartUploading` to a `PartUploaded`.
    --
    -- XXX Pattern match could fail...
    newUploadParts :: UT.UploadTree backend S3.UploadPartResponse
    Just newUploadParts = UT.replace finishedPartNum finishedPart uploadParts

    finishedPart = UT.PartUploaded finishedPartNum response

    toCompleted :: UT.Part backend S3.UploadPartResponse -> Maybe S3.CompletedPart
    toCompleted (UT.PartUploaded{getPartNumber = (UT.PartNumber getPartNumber), getPartResponse}) = completed
      where
        -- Have to convert the Integer part number to an Int for S3.  S3 only
        -- supports part numbers up to 10,000 so this should be safe ...
        completed = S3.newCompletedPart (fromIntegral getPartNumber) <$> (getPartResponse ^. S3.uploadPartResponse_eTag)
    toCompleted _ = Nothing

{- | Instantiate a new S3 backend which will interact with objects in the
 given bucket using the given AWS Env.
-}
newS3Backend :: forall backend delay. AWS.Env -> S3.BucketName -> T.Text -> IO (S3Backend backend delay)
newS3Backend s3BackendEnv s3BackendBucket s3BackendPrefix = do
    -- XXX Load existing s3BackendState from S3 itself.  List the
    -- MultipartUploads and figure out what is in progress.  Also need to
    -- encode uploadStateSize and uploadSecret in the MultipartUpload somehow,
    -- probably.
    s3BackendState <- atomically SMap.empty

    pure S3Backend{..}

{- | The maximum S3 object size is 5 TB.  We currently add no bookkeeping
 overhead so the client-supplied share can use all of this.
-}
maxShareSize :: Integer
maxShareSize = 5 * 2 ^ (40 :: Integer)

{- | Compute the threshold for the size of a contiguous chunk of buffered data
 associated with an immutable upload which, when crossed, triggers an upload
 of that buffered data to S3 as a "PartUpload".

 There is a limit of 10,000 "parts" per "MultipartUpload" so we related the
 share size to both the maximum share size and this limit.
-}
partUploadThreshold :: Size -> Size
partUploadThreshold = (`div` 10_000)

{- | The number of microseconds which must pass with no progress made
 uploading any of a number of shares allocated together before the server
 will consider the upload failed and garbage collect related state.
-}
immutableUploadProgressTimeout :: Int
immutableUploadProgressTimeout = 1_000_000 * 600

{- | Something has gone wrong with the backend that does not fit into the
 generic exceptions defined by tahoe-great-black-swamp.
-}
data BackendError
    = ShareAllocationIncomplete
    deriving (Eq, Show)

instance Exception BackendError

{- | A storage backend which keeps shares in objects in buckets behind and
 S3-compatible API.

  The backend is configured with credentials to use the API and the name of a
  bucket to use.

  Uploads for immutable objects are handled using S3 "multipart uploads" to
  send the data from each write made on to us onwards to S3 as a separate
  part.  The overall upload is completed when the last write is received.

  Objects are given S3 keys like `<storage index>/<share number>`.
-}
instance forall backend delay. (UT.IsBackend backend, HasDelay delay) => Backend (S3Backend backend delay) where
    version _ = pure $ Version params appVer
      where
        params =
            Version1Parameters
                { maximumImmutableShareSize = maxShareSize
                , maximumMutableShareSize = maxShareSize
                , -- Just estimate ...
                  availableSpace = 2 ^ (50 :: Integer)
                }

        appVer = "tahoe-s3/0.0.0.0"

    -- Open a logical transaction for creating some new shares at a certain
    -- storage index.
    --
    -- For each share number, this can succeed if and only if that share
    -- number has not already been created at the given storage index.
    --
    -- Each share number for which this operation succeeds will be included in
    -- the "allocated" set of the result.
    --
    -- Each share number for which creation has already *completed* will be
    -- included in the "alreadyHave" set of the result.
    --
    -- Each share number which is in the process of being created will be
    -- included in neither set.
    --
    -- Creation may also fail because of an error from the S3 server.  In this
    -- case, the affected share's state is not changed but if there were other
    -- shares mentioned in the request their state may be changed.
    createImmutableStorageIndex :: S3Backend backend delay -> StorageIndex -> Maybe [LeaseSecret] -> AllocateBuckets -> IO AllocationResult
    createImmutableStorageIndex s3@S3Backend{..} storageIndex secrets AllocateBuckets{..}
        | allocatedSize > maxShareSize = throwIO $ MaximumShareSizeExceeded maxShareSize allocatedSize
        | otherwise =
            -- Creation may also fail because no upload secret has been given.
            withUploadSecret secrets $ \uploadSecret -> do
                delayed <- new :: IO delay

                -- Try to allocate each share from the request.
                results <-
                    mapConcurrently
                        (try @SomeException . createOneImmutableShare s3 storageIndex allocatedSize uploadSecret delayed)
                        shareNumbers

                -- Combine all of the successful allocations for the response
                -- to the caller.  We expect that an errors encountered are
                -- already handled sufficiently to leave state consistent.
                let result = fold . rights $ results

                -- wait on the delay and then clean up the resources allocated
                -- for these uploads
                void $ forkIO $ cleanupAfterProgressTimeout immutableUploadProgressTimeout delayed (cleanup result)

                pure result
      where
        cleanupAfterProgressTimeout n delayed cleanup' = do
            res <- race (wait delayed) (delay' delayed n)
            case res of
                Left (Delayed n') ->
                    -- The timeout was delayed.  Begin again.
                    cleanupAfterProgressTimeout n' delayed cleanup'
                Left Cancelled ->
                    -- The timeout was cancelled.  We're done.
                    pure ()
                Right _ ->
                    -- The timeout elapsed and we cleaned up.  We're done.
                    cleanup'

        -- Clean up state related to the given AllocationResult because the
        -- upload was aborted or timed out.
        cleanup result = do
            externalState <- atomically $ traverse (cleanupInternalImmutable s3 storageIndex) (allocated result)

            let f :: (ShareNumber, S3.CreateMultipartUploadResponse) -> ResourceT IO ()
                f = uncurry $ cleanupExternalImmutable s3 storageIndex

                xs :: [(ShareNumber, S3.CreateMultipartUploadResponse)]
                xs = mapMaybe sequenceA (zip (allocated result) externalState)

            runResourceT $ traverse_ f xs

    writeImmutableShare :: S3Backend backend delay -> StorageIndex -> ShareNumber -> Maybe [LeaseSecret] -> ShareData -> QueryRange -> IO ()
    writeImmutableShare _ _ _ Nothing _ _ = throwIO MissingUploadSecret
    writeImmutableShare s3 storageIndex shareNum secrets shareData Nothing = do
        -- If no range is given, this is the whole thing.
        writeImmutableShare s3 storageIndex shareNum secrets shareData (Just [ByteRangeFromTo 0 (fromIntegral $ B.length shareData - 1)])
    writeImmutableShare s3Backend storageIndex shareNum secrets shareData (Just byteRanges) =
        traverse_ (uncurry (writeAnImmutableChunk s3Backend storageIndex shareNum secrets)) (divideByRanges byteRanges shareData)

    abortImmutableUpload :: S3Backend backend delay -> StorageIndex -> ShareNumber -> Maybe [LeaseSecret] -> IO ()
    abortImmutableUpload _ _ _ Nothing = throwIO MissingUploadSecret
    abortImmutableUpload (S3Backend{..}) storageIndex shareNum secrets = do
        -- Try to find the matching upload state and discard it if the secret matches.
        toCancel <- atomically $ adjust' internalCancel stateKey s3BackendState
        -- If we found it, also cancel the state in the S3 server.
        case toCancel of
            Nothing -> throwIO IncorrectUploadSecret
            Just (progressTimeout, uploadId) -> do
                cancelMultipartUpload uploadId
                cancel progressTimeout
      where
        stateKey = (storageIndex, shareNum)

        cancelMultipartUpload uploadId = runResourceT $ do
            -- XXX Check the response for errors
            _resp <- AWS.send s3BackendEnv $ S3.newAbortMultipartUpload s3BackendBucket (storageIndexShareNumberToObjectKey s3BackendPrefix storageIndex shareNum) uploadId
            pure ()

        internalCancel UploadState{uploadResponse, uploadSecret, uploadProgressTimeout}
            | validUploadSecret uploadSecret secrets =
                maybe (throw ShareAllocationIncomplete) ((Nothing,) . (uploadProgressTimeout,) . (^. S3.createMultipartUploadResponse_uploadId)) uploadResponse
            | otherwise = throw IncorrectUploadSecret

    getImmutableShareNumbers (S3Backend{s3BackendEnv, s3BackendBucket, s3BackendPrefix}) storageIndex = runResourceT $ do
        resp <- AWS.send s3BackendEnv (set S3.listObjects_prefix (Just . storageIndexPrefix s3BackendPrefix $ storageIndex) $ S3.newListObjects s3BackendBucket)
        case view S3.listObjectsResponse_contents resp of
            Nothing -> pure $ CBORSet mempty
            Just objects -> pure $ CBORSet shareNumbers
              where
                shareNumbers = Set.fromList (mapMaybe objToShareNum objects)

                objToShareNum :: S3.Object -> Maybe ShareNumber
                objToShareNum obj =
                    case parsed of
                        [prefix, si, shareNum] ->
                            if prefix == s3BackendPrefix && T.unpack si == storageIndex
                                then ShareNumber <$> readMaybe (T.unpack shareNum)
                                else Nothing
                        _ -> Nothing
                  where
                    parsed = T.split (== '/') (view (S3.object_key . S3._ObjectKey) obj)

    readImmutableShare :: S3Backend backend delay -> StorageIndex -> ShareNumber -> QueryRange -> IO ShareData
    readImmutableShare (S3Backend{s3BackendEnv, s3BackendBucket, s3BackendPrefix}) storageIndex shareNum qrange =
        runResourceT $ do
            huh <- readEach qrange
            pure $ B.concat huh
      where
        objectKey = storageIndexShareNumberToObjectKey s3BackendPrefix storageIndex shareNum
        readEach :: Maybe [ByteRange] -> ResourceT IO [ShareData]
        readEach Nothing = do
            resp <- AWS.send s3BackendEnv (S3.newGetObject s3BackendBucket objectKey)
            pure . B.concat <$> AWS.sinkBody (resp ^. S3.getObjectResponse_body) sinkList
        readEach (Just ranges) = mapM readOne ranges
        readOne :: ByteRange -> ResourceT IO ShareData
        readOne br = do
            let getObj = set S3.getObject_range (Just . ("bytes=" <>) . T.pack . C8.unpack $ renderByteRange br) $ S3.newGetObject s3BackendBucket objectKey
            resp <- AWS.send s3BackendEnv getObj
            B.concat <$> AWS.sinkBody (resp ^. S3.getObjectResponse_body) sinkList

    readvAndTestvAndWritev :: HasCallStack => S3Backend backend delay -> StorageIndex -> WriteEnablerSecret -> ReadTestWriteVectors -> IO ReadTestWriteResult
    readvAndTestvAndWritev s3@S3Backend{s3BackendPrefix, s3BackendBucket, s3BackendEnv} storageIndex secret (ReadTestWriteVectors{readVector, testWriteVectors}) =
        -- XXX
        --
        -- 1. Check results from S3 for errors
        --
        -- 2. Chunk data somehow to avoid catastrophic performance problems
        -- from the current strategy of whole-object updates for every write.
        --
        case testWriteVectorList of
            [] ->
                -- There are no writes so skip the secret check and the test/write logic.
                ReadTestWriteResult True <$> doReads
            (arbShareNum, _) : _ -> do
                expectedSecret <-
                    ( metadataWriteEnabler . fst
                            <$> readShare s3 storageIndex arbShareNum (Just [ByteRangeFromTo 0 0])
                        )
                        `catch` on404 Nothing

                when (isJust expectedSecret && Just secret /= expectedSecret) $ throwIO IncorrectWriteEnablerSecret

                (readResults, testResults) <- concurrently doReads doTests
                if and . fmap and $ testResults
                    then ReadTestWriteResult True readResults <$ doWrites
                    else pure $ ReadTestWriteResult False readResults
      where
        testWriteVectorList = Map.toList testWriteVectors

        doReads = do
            (CBORSet knownShareNumbers) <- getMutableShareNumbers s3 storageIndex
            let shareNumberList = Set.toList knownShareNumbers
            shareData <- mapConcurrently id $ readMutableShare s3 storageIndex <$> shareNumberList <*> pure (toQueryRange readVector)
            pure $ Map.fromList (zip shareNumberList ((: []) <$> shareData))

        toQueryRange = Just . fmap toSingleRange
          where
            toSingleRange (ReadVector offset size) = ByteRangeFromTo offset (offset + size - 1)

        doTests =
            mapConcurrently
                (\(shareNum, testWriteVectors') -> zipWith runTestVector (test testWriteVectors') <$> mapM (readSomeThings shareNum) (test testWriteVectors'))
                testWriteVectorList

        doWrites =
            mapConcurrently_
                (\(shareNum, testWriteVectors') -> readEverything shareNum >>= writeEverything shareNum . (`applyWriteVectors` write testWriteVectors'))
                testWriteVectorList

        runTestVector (TestVector _ _ Eq specimen) shareData = specimen == shareData
        runTestVector _ _ = False

        readSomeThings shareNum (TestVector offset size _ _) =
            readMutableShare s3 storageIndex shareNum (Just [ByteRangeFromTo offset (offset + size - 1)])
                `catch` on404 ""

        readEverything shareNum =
            readMutableShare s3 storageIndex shareNum Nothing `catch` on404 ""

        writeEverything shareNum bs =
            runResourceT $
                AWS.send
                    s3BackendEnv
                    ( (S3.putObject_metadata .~ HashMap.singleton writeEnablerSecretMetadataKey (base64WriteEnabler secret)) $
                        S3.newPutObject
                            s3BackendBucket
                            (storageIndexShareNumberToObjectKey s3BackendPrefix storageIndex shareNum)
                            (AWS.toBody bs)
                    )

        base64WriteEnabler (WriteEnablerSecret bytes) = T.decodeLatin1 . Base64.encode $ bytes

    getMutableShareNumbers = getImmutableShareNumbers

    readMutableShare :: S3Backend backend delay -> StorageIndex -> ShareNumber -> QueryRange -> IO ShareData
    readMutableShare = readImmutableShare

{- | Given the complete bytes of a share, apply a single write vector and
 return the modified bytes.
-}
applyWriteVector :: B.ByteString -> WriteVector -> B.ByteString
applyWriteVector bs (WriteVector offset shareData) =
    prefix <> filler <> shareData <> suffix
  where
    prefix = B.take (fromIntegral offset) bs
    suffix = B.drop (fromIntegral offset + B.length shareData) bs
    filler = B.replicate (fromIntegral offset - B.length bs) 0

-- | Given the complete bytes of a share, apply a
applyWriteVectors :: Foldable f => B.ByteString -> f WriteVector -> B.ByteString
applyWriteVectors = foldl' applyWriteVector

storageIndexShareNumberToObjectKey :: T.Text -> StorageIndex -> ShareNumber -> S3.ObjectKey
storageIndexShareNumberToObjectKey prefix si (ShareNumber sn) =
    S3.ObjectKey $ T.concat [storageIndexPrefix prefix si, T.pack (show sn)]

storageIndexPrefix :: T.Text -> StorageIndex -> T.Text
storageIndexPrefix prefix si = T.concat [prefix, "/", T.pack si, "/"]

{-
the remaining methods to implement:
class Backend b where
    -- | Update the lease expiration time on the shares associated with the
    -- given storage index.
    renewLease :: b -> StorageIndex -> [LeaseSecret] -> IO ()

    adviseCorruptImmutableShare :: b -> StorageIndex -> ShareNumber -> CorruptionDetails -> IO ()
    adviseCorruptMutableShare :: b -> StorageIndex -> ShareNumber -> CorruptionDetails -> IO ()
-}

{- | If a map includes the given key, compute a new value and a result value for
 the current value.  If the new computed value is Nothing the key is removed.
 If the map does not include the key, do nothing.  Otherwise, replace the old
 value with the new value and return the result value.
-}
adjust' :: Hashable k => (v -> (Maybe v, b)) -> k -> SMap.Map k v -> STM (Maybe b)
adjust' f k m = do
    SMap.lookup k m >>= \case
        Nothing -> pure Nothing
        Just v -> do
            maybe (SMap.delete k m) (\v' -> SMap.insert k v' m) newV
            pure $ Just result
          where
            (newV, result) = f v

validUploadSecret :: UploadSecret -> Maybe [LeaseSecret] -> Bool
validUploadSecret us1 (Just [Upload us2]) = us1 == us2
validUploadSecret _ _ = False

data Metadata = Metadata
    { metadataWriteEnabler :: Maybe WriteEnablerSecret
    }

readShare :: S3Backend backend delay -> StorageIndex -> ShareNumber -> QueryRange -> IO (Metadata, ShareData)
readShare (S3Backend{s3BackendEnv, s3BackendBucket, s3BackendPrefix}) storageIndex shareNum qrange =
    runResourceT $ second B.concat <$> readEach qrange
  where
    objectKey = storageIndexShareNumberToObjectKey s3BackendPrefix storageIndex shareNum

    readEach :: Maybe [ByteRange] -> ResourceT IO (Metadata, [ShareData])
    readEach Nothing = do
        resp <- AWS.send s3BackendEnv (S3.newGetObject s3BackendBucket objectKey)
        body <- AWS.sinkBody (resp ^. S3.getObjectResponse_body) sinkList
        pure (readMetadata resp, body)
    readEach (Just []) = pure (Metadata Nothing, [])
    readEach (Just ranges) = do
        (metadata : _, shareData) <- unzip <$> mapM readOne ranges
        pure (metadata, shareData)

    readOne :: ByteRange -> ResourceT IO (Metadata, ShareData)
    readOne br = do
        let getObj =
                set S3.getObject_range (Just . ("bytes=" <>) . T.pack . C8.unpack $ renderByteRange br) $
                    S3.newGetObject s3BackendBucket objectKey
        resp <- AWS.send s3BackendEnv getObj
        body <- B.concat <$> AWS.sinkBody (resp ^. S3.getObjectResponse_body) sinkList
        pure (readMetadata resp, body)

writeEnablerSecretMetadataKey :: T.Text
writeEnablerSecretMetadataKey = "write-enabler-secret"

readMetadata :: S3.GetObjectResponse -> Metadata
readMetadata resp =
    Metadata
        { metadataWriteEnabler = (either (const Nothing) (Just . WriteEnablerSecret) . Base64.decode . T.encodeUtf8) =<< HashMap.lookup writeEnablerSecretMetadataKey (resp ^. S3.getObjectResponse_metadata)
        }

on404 :: a -> AWS.Error -> IO a
on404 result (AWS.ServiceError AWS.ServiceError'{status = Status{statusCode = 404}}) = pure result
on404 _ e = throwIO e

{- | Allocate tracking state, locally and in the S3 server, for a new share of
 the given size at the given location.
-}
createOneImmutableShare ::
    UT.IsBackend backend =>
    S3Backend backend delay ->
    StorageIndex ->
    Integer ->
    UploadSecret ->
    delay ->
    ShareNumber ->
    IO AllocationResult
createOneImmutableShare s3@S3Backend{..} storageIndex allocatedSize uploadSecret delay shareNum = do
    -- Initialize local tracking state
    allocated <- atomically $ internalAllocate storageIndex uploadSecret allocatedSize shareNum delay s3BackendState

    if allocated
        then do
            -- Create corresponding state in the S3 server.
            response <-
                runResourceT (externalAllocate s3 storageIndex shareNum) `onException` do
                    atomically (undoInternalAllocate storageIndex shareNum s3BackendState)

            -- Update the local state with the details of the allocations
            -- on the S3 server.  We need the multipart upload identifier
            -- to associate future part uploads with the right thing.
            atomically $ internalAllocateComplete storageIndex shareNum response s3BackendState

            pure mempty{allocated = [shareNum]}
        else pure mempty{alreadyHave = [shareNum]}

data CleanupError = InternalImmutableStateMissing deriving (Eq, Show)

instance Exception CleanupError

{- | Discard internal partial-upload state related to the given shares.
 Return information about their associated external partial-upload state for
 separate cleanup.
-}
cleanupInternalImmutable :: S3Backend backend delay -> StorageIndex -> ShareNumber -> STM (Maybe S3.CreateMultipartUploadResponse)
cleanupInternalImmutable S3Backend{..} storageIndex shareNum =
    SMap.lookup (storageIndex, shareNum) s3BackendState >>= \case
        Nothing -> throwSTM InternalImmutableStateMissing
        Just state -> do
            SMap.delete (storageIndex, shareNum) s3BackendState
            -- XXX What if we don't have the response yet?
            pure (uploadResponse state)

{- | Discard external partial-upload state related to the given multipart
 upload responses.
-}
cleanupExternalImmutable :: S3Backend backend delay -> StorageIndex -> ShareNumber -> S3.CreateMultipartUploadResponse -> ResourceT IO ()
cleanupExternalImmutable S3Backend{..} storageIndex shareNum createResponse =
    void $ AWS.send s3BackendEnv abortRequest
  where
    abortRequest = S3.newAbortMultipartUpload s3BackendBucket objKey uploadId
    objKey = storageIndexShareNumberToObjectKey s3BackendPrefix storageIndex shareNum
    uploadId = createResponse ^. S3.createMultipartUploadResponse_uploadId

writeAnImmutableChunk :: (UT.IsBackend backend, HasDelay delay) => S3Backend backend delay -> StorageIndex -> ShareNumber -> Maybe [LeaseSecret] -> Offset -> ShareData -> IO ()
writeAnImmutableChunk s3backend@(S3Backend{s3BackendEnv, s3BackendBucket, s3BackendPrefix, s3BackendState}) storageIndex shareNum secrets offset shareData =
    do
        -- call list objects on the backend, if this share already exists, reject the new write with ImmutableShareAlreadyWritten
        CBORSet shareNumbers <- getImmutableShareNumbers s3backend storageIndex
        when (shareNum `elem` shareNumbers) $ throwIO ImmutableShareAlreadyWritten

        -- If possible, update internal state to reflect this new piece of
        -- share data.  If this new data is large enough on its own, or
        -- combines with some already-written data to exceed the threshold,
        -- being a new part upload and record its details.
        uploadInfo <- atomically (internalStartUpload secrets storageIndex shareNum (trace ("offset: " <> show offset <> " size: " <> show (B.length shareData)) offset) shareData s3BackendState)

        case uploadInfo of
            Nothing -> throwIO ImmutableShareAlreadyWritten
            Just NotReady -> throwIO ShareAllocationIncomplete
            Just Collision -> throwIO ConflictingWrite
            Just Buffering -> updateTimeout
            Just (StartUpload (UT.PartNumber partNum') uploadId dataToUpload) -> do
                updateTimeout

                let objKey = storageIndexShareNumberToObjectKey s3BackendPrefix storageIndex shareNum
                    uploadPart = S3.newUploadPart s3BackendBucket objKey (fromIntegral partNum') uploadId (AWS.toBody dataToUpload)
                -- XXX This could, of course, fail...  Then we have some state to roll back.
                response <- runResourceT $ AWS.send s3BackendEnv uploadPart
                newState <- atomically $ internalFinishUpload partNum' shareData storageIndex shareNum response s3BackendState
                case newState of
                    Nothing ->
                        error "It went missing?"
                    Just (True, []) ->
                        error "No parts upload but we're done, huh?"
                    Just (True, p : parts) -> do
                        let completedMultipartUpload = (S3.completedMultipartUpload_parts ?~ (p :| parts)) S3.newCompletedMultipartUpload
                            completeMultipartUpload =
                                (S3.completeMultipartUpload_multipartUpload ?~ completedMultipartUpload)
                                    (S3.newCompleteMultipartUpload s3BackendBucket objKey uploadId)

                        -- XXX Oops, this could fail too.
                        void $ runResourceT $ AWS.send s3BackendEnv completeMultipartUpload
                    Just (False, _) -> pure ()
  where
    updateTimeout = do
        s <- atomically $ SMap.lookup (storageIndex, shareNum) s3BackendState
        case s of
            Nothing -> pure ()
            Just UploadState{..} -> update uploadProgressTimeout immutableUploadProgressTimeout

{- | Divide a single ByteString of data into pieces based on a list of ranges,
 keeping track of the offset of the start of each resulting piece relative
 to the original ByteString.
-}
divideByRanges :: [ByteRange] -> ShareData -> [(Offset, ShareData)]
divideByRanges [] "" = []
divideByRanges [] _ = error "There are no more ranges but there is still share data"
divideByRanges (_ : _) "" = error "There are more ranges but we ran out of share data"
divideByRanges (ByteRangeFromTo from to : rs) shareData = (from, B.take size shareData) : divideByRanges rs (B.drop size shareData)
  where
    size = fromIntegral $ to - from + 1
divideByRanges (_ : _) _ = error "Only `from-to` ranges are supported for uploads"
