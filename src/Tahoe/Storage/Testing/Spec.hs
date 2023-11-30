{-# OPTIONS_GHC -Wno-orphans #-}

module Tahoe.Storage.Testing.Spec (
    ShareNumbers (..),
    makeStorageSpec,
    genStorageIndex,
) where

import Control.Exception (Exception, throwIO, try)
import Control.Monad (void, when)
import qualified Data.Base32String as Base32
import Data.Bits (Bits (xor))
import qualified Data.ByteString as B
import Data.Composition ((.:))
import Data.Interval (
    Boundary (Closed, Open),
    Extended (..),
    Interval,
    interval,
    lowerBound,
    upperBound,
 )
import qualified Data.IntervalSet as IS
import Data.List.HT (outerProduct)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Word (Word8)
import Network.HTTP.Types (ByteRange (..), ByteRanges)
import Tahoe.Storage.Backend (
    AllocateBuckets (AllocateBuckets),
    AllocationResult (AllocationResult, allocated, alreadyHave),
    Backend (..),
    CBORSet (CBORSet),
    LeaseSecret (Upload),
    Offset,
    ReadResult,
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
    UploadSecret (..),
    Version (parameters),
    Version1Parameters (maximumImmutableShareSize),
    WriteEnablerSecret (..),
    WriteImmutableError (..),
    WriteMutableError (..),
    WriteVector (..),
    readv,
    testv,
    writev,
 )
import Test.Hspec (Expectation, HasCallStack, Selector, Spec, context, describe, it, shouldBe, shouldReturn, shouldThrow)
import Test.QuickCheck (
    Arbitrary (arbitrary),
    Gen,
    NonEmptyList (NonEmpty, getNonEmpty),
    NonNegative (NonNegative, getNonNegative),
    Positive (Positive, getPositive),
    Property,
    Testable (property),
    chooseInteger,
    counterexample,
    forAll,
    ioProperty,
    listOf1,
    oneof,
    shuffle,
    sublistOf,
    suchThatMap,
    vector,
    vectorOf,
    withMaxSuccess,
    (==>),
 )
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Monadic (monadicIO, run)

arbNonNeg :: (Arbitrary n, Integral n) => Gen n
arbNonNeg = getNonNegative <$> arbitrary

newtype ShareNumbers = ShareNumbers {getShareNumbers :: [ShareNumber]} deriving (Eq, Ord, Show)

{- | An Arbitrary instance that guarantees ShareNumbers are unique and
   non-empty (without invoking discard).
-}
instance Arbitrary ShareNumbers where
    arbitrary = ShareNumbers . fmap ShareNumber <$> nums
      where
        nums =
            arbitrary
                >>= (shuffle . enumFromTo 0) . getNonNegative
                >>= \(num : rest) -> (num :) <$> sublistOf rest

instance Arbitrary ShareNumber where
    arbitrary = ShareNumber <$> arbNonNeg

instance Arbitrary ReadTestWriteVectors where
    arbitrary = ReadTestWriteVectors <$> arbitrary <*> arbitrary

instance Arbitrary TestWriteVectors where
    arbitrary = TestWriteVectors <$> arbitrary <*> arbitrary <*> oneof [pure Nothing, Just <$> arbNonNeg]

instance Arbitrary TestVector where
    arbitrary = TestVector <$> arbNonNeg <*> arbNonNeg <*> pure Eq <*> arbitrary

instance Arbitrary WriteVector where
    arbitrary = WriteVector <$> arbNonNeg <*> arbitrary

instance Arbitrary ReadVector where
    arbitrary = ReadVector <$> arbNonNeg <*> (getPositive <$> arbitrary)

newtype ArbStorageIndex = ArbStorageIndex StorageIndex deriving newtype (Show)

instance Arbitrary ArbStorageIndex where
    arbitrary = ArbStorageIndex <$> genStorageIndex

newtype SomeShareData = SomeShareData {getShareData :: B.ByteString} deriving (Show)

instance Arbitrary SomeShareData where
    arbitrary = SomeShareData . B.pack <$> listOf1 (arbitrary :: Gen Word8)

b32table :: B.ByteString
b32table = "abcdefghijklmnopqrstuvwxyz234567"

b32encode :: B.ByteString -> String
b32encode = T.unpack . Base32.toText . Base32.fromBytes b32table

genStorageIndex :: Gen StorageIndex
genStorageIndex =
    suchThatMap gen10ByteString (Just . b32encode)

gen10ByteString :: Gen B.ByteString
gen10ByteString =
    suchThatMap (vectorOf 10 (arbitrary :: Gen Word8)) (Just . B.pack)

shouldThrowAndShow :: forall e a. (HasCallStack, Exception e, Show a) => IO a -> Selector e -> Expectation
shouldThrowAndShow action selector = do
    result <- try action :: IO (Either e a)
    case result of
        Left exc -> throwIO exc `shouldThrow` selector
        Right value -> do
            print $ "Got a value instead of an exception: " <> show value

{- | Instantiate property tests for the storage backend specification for a
 particular backend.
-}
makeStorageSpec ::
    Backend b =>
    -- | An action that produces a new, empty backend.
    IO b ->
    -- | A function that produces an action to clean up any state that may
    -- have been created in the given backend.
    (b -> IO ()) ->
    -- | A test specification for the backend.
    Spec
makeStorageSpec makeBackend cleanupBackend = do
    let runBackend = withBackend makeBackend cleanupBackend
    context "v1" $ do
        context "immutable" $ do
            describe "allocate a storage index" $ do
                it "rejects allocations above the immutable share size limit" $
                    withMaxSuccess few $ \(ArbStorageIndex storageIndex) (ShareNumbers shareNums) secret (Positive extra) -> do
                        runBackend $ \backend -> do
                            limit <- maximumImmutableShareSize . parameters <$> version backend
                            createImmutableStorageIndex backend storageIndex (Just [Upload (UploadSecret secret)]) (AllocateBuckets shareNums (limit + extra))
                                `shouldThrow` (== MaximumShareSizeExceeded limit (limit + extra))

                it "accounts for all allocated share numbers" $
                    property $
                        forAll genStorageIndex (alreadyHavePlusAllocatedImm runBackend)

            describe "write a share" $ do
                it "disallows writing an unallocated share" $
                    withMaxSuccess few $ \(ArbStorageIndex storageIndex) shareNum secret (SomeShareData shareData) ->
                        runBackend $ \backend -> do
                            writeImmutableShare backend storageIndex shareNum (Just [Upload (UploadSecret secret)]) shareData Nothing
                                `shouldThrow` (== ShareNotAllocated)

                it "disallows writes without an upload secret" $
                    property $
                        runBackend $ \backend -> do
                            AllocationResult [] [ShareNumber 0] <- createImmutableStorageIndex backend "storageindex" (Just [anUploadSecret]) (AllocateBuckets [ShareNumber 0] 100)
                            writeImmutableShare backend "storageindex" (ShareNumber 0) Nothing "fooooo" Nothing `shouldThrow` (== MissingUploadSecret)

                it "disallows writes without a matching upload secret" $
                    property $
                        runBackend $ \backend -> do
                            AllocationResult [] [ShareNumber 0] <- createImmutableStorageIndex backend "storageindex" (Just [anUploadSecret]) (AllocateBuckets [ShareNumber 0] 100)
                            -- Supply the wrong secret as an upload secret and the
                            -- right secret marked for some other use - this
                            -- should still fail.
                            writeImmutableShare backend "storageindex" (ShareNumber 0) (Just [Upload (UploadSecret "wrongsecret")]) "fooooo" Nothing `shouldThrow` (== IncorrectUploadSecret)

                it "returns the share numbers that were written" $
                    property $
                        forAll genStorageIndex (immutableWriteAndEnumerateShares runBackend)

                it "returns the written data when requested" $
                    property $
                        forAll genStorageIndex (immutableWriteAndReadShare runBackend)

                it "cannot be written more than once" $
                    property $
                        forAll genStorageIndex (immutableWriteAndRewriteShare runBackend)

        describe "aborting uploads" $ do
            it "disallows aborts without an upload secret" $
                property $
                    runBackend $ \backend -> do
                        abortImmutableUpload backend "storageindex" (ShareNumber 0) Nothing `shouldThrow` (== MissingUploadSecret)

            it "disallows upload completion after a successful abort" $
                withMaxSuccess few $ \(ArbStorageIndex storageIndex) shareNum secret (SomeShareData shareData) size ->
                    runBackend $ \backend -> do
                        void $ createImmutableStorageIndex backend storageIndex (Just [Upload (UploadSecret secret)]) (AllocateBuckets [shareNum] size)
                        abortImmutableUpload backend storageIndex shareNum (Just [Upload (UploadSecret secret)])
                        writeImmutableShare backend storageIndex shareNum (Just [Upload (UploadSecret secret)]) shareData Nothing
                            `shouldThrow` (== ShareNotAllocated)

            it "disallows aborts without a matching upload secret" $
                property $
                    runBackend $ \backend -> do
                        AllocationResult [] [ShareNumber 0] <- createImmutableStorageIndex backend "storageindex" (Just [anUploadSecret]) (AllocateBuckets [ShareNumber 0] 100)
                        abortImmutableUpload backend "storageindex" (ShareNumber 0) (Just [Upload (UploadSecret "wrongsecret")]) `shouldThrow` (== IncorrectUploadSecret)

            it "allows aborts with a matching upload secret" $
                property $
                    runBackend $ \backend -> do
                        AllocationResult [] [ShareNumber 0] <- createImmutableStorageIndex backend "storageindex" (Just [anUploadSecret]) (AllocateBuckets [ShareNumber 0] 100)
                        abortImmutableUpload backend "storageindex" (ShareNumber 0) (Just [anUploadSecret])

        context "mutable" $ do
            -- XXX There's lots of problems around supplying negative integer
            -- values in most places.  We avoid tripping over those cases here
            -- but we should really fix the implementation to deal with them
            -- sensible.
            describe "write a share" $ do
                it "returns the share numbers that were written" $
                    property $
                        forAll genStorageIndex (mutableWriteAndEnumerateShares runBackend)

                it "rejects an update with the wrong write enabler" $
                    forAll genStorageIndex $ \storageIndex shareNum (secret, wrongSecret) (SomeShareData shareData, SomeShareData junkData) (NonNegative offset) ->
                        (secret /= wrongSecret)
                            && (shareData /= junkData)
                            ==> monadicIO
                                . run
                                . runBackend
                            $ \backend -> do
                                first <- readvAndTestvAndWritev backend storageIndex (WriteEnablerSecret secret) (writev shareNum offset shareData)
                                success first `shouldBe` True
                                readvAndTestvAndWritev backend storageIndex (WriteEnablerSecret wrongSecret) (writev shareNum offset junkData)
                                    `shouldThrowAndShow` (== IncorrectWriteEnablerSecret)
                                third <- readvAndTestvAndWritev backend storageIndex (WriteEnablerSecret secret) (readv offset (fromIntegral $ B.length shareData))
                                readData third `shouldBe` Map.singleton shareNum [shareData]

                it "returns the written data when requested" $
                    forAll genStorageIndex (mutableWriteAndReadShare runBackend)

                it "overwrites older data with newer data" $
                    -- XXX We go out of our way to generate a legal storage
                    -- index here.  Illegal storage indexes aren't checked by
                    -- the system anywhere but they really ought to be.
                    forAll genStorageIndex $ \storageIndex (readVectors :: NonEmptyList ReadVector) secret shareNum -> do
                        let is = readVectorToIntervalSet (getNonEmpty readVectors)
                            sp = IS.span is
                            (lower, upper) = toFiniteBounds sp
                            size = upper - lower
                        bs <- B.pack <$> vector (fromIntegral size)
                        writeVectors <- writesThatResultIn bs lower size
                        pure $
                            counterexample ("write vectors: " <> show writeVectors) $
                                ioProperty $
                                    runBackend $ \backend -> do
                                        let x = foldMap (\(WriteVector off shareData) -> writev shareNum off shareData) writeVectors
                                        writeResult <- readvAndTestvAndWritev backend storageIndex (WriteEnablerSecret secret) x
                                        success writeResult `shouldBe` True

                                        let y = foldMap (\(ReadVector off sz) -> readv off sz) (getNonEmpty readVectors)
                                        readResult <- readvAndTestvAndWritev backend storageIndex (WriteEnablerSecret secret) y
                                        Map.map B.concat (readData readResult)
                                            `shouldBe` Map.singleton shareNum (B.concat $ extractRead lower bs <$> getNonEmpty readVectors)

                it "accepts writes for which the test condition succeeds" $
                    withMaxSuccess few $ \(ArbStorageIndex storageIndex) secret ->
                        runBackend $ \backend -> do
                            runReadTestWrite_ backend storageIndex (WriteEnablerSecret secret) (writev (ShareNumber 0) 0 "abc")
                            runReadTestWrite_ backend storageIndex (WriteEnablerSecret secret) (testv (ShareNumber 0) 0 "abc" <> writev (ShareNumber 0) 0 "xyz")
                            readMutableShare backend storageIndex (ShareNumber 0) Nothing `shouldReturn` "xyz"

                it "rejects writes for which the test condition fails" $
                    withMaxSuccess few $ \(ArbStorageIndex storageIndex) secret ->
                        runBackend $ \backend -> do
                            runReadTestWrite_ backend storageIndex (WriteEnablerSecret secret) (writev (ShareNumber 0) 0 "abc")
                            runReadTestWrite backend storageIndex (WriteEnablerSecret secret) (testv (ShareNumber 0) 0 "abd" <> writev (ShareNumber 0) 0 "xyz")
                                `shouldThrow` (\WriteRefused{} -> True)
                            readMutableShare backend storageIndex (ShareNumber 0) Nothing `shouldReturn` "abc"

                it "retrieves share data from before writes are applied" $ do
                    withMaxSuccess few $ \(ArbStorageIndex storageIndex) secret ->
                        runBackend $ \backend -> do
                            runReadTestWrite_ backend storageIndex (WriteEnablerSecret secret) (writev (ShareNumber 0) 0 "abc")
                            runReadTestWrite backend storageIndex (WriteEnablerSecret secret) (readv 0 3 <> writev (ShareNumber 0) 0 "xyz")
                                `shouldReturn` Map.fromList [(ShareNumber 0, ["abc"])]
                            runReadTestWrite backend storageIndex (WriteEnablerSecret secret) (readv 0 3)
                                `shouldReturn` Map.fromList [(ShareNumber 0, ["xyz"])]

alreadyHavePlusAllocatedImm ::
    Backend b =>
    ((b -> IO ()) -> IO ()) -> -- Execute a function on the backend.
    StorageIndex -> -- The storage index to use
    ShareNumbers -> -- The share numbers to allocate
    Positive Size -> -- The size of each share
    Property
alreadyHavePlusAllocatedImm runBackend storageIndex (ShareNumbers shareNumbers) (Positive size) = monadicIO $
    run $
        runBackend $ \backend -> do
            result <- createImmutableStorageIndex backend storageIndex (Just [anUploadSecret]) $ AllocateBuckets shareNumbers size
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
    ((b -> IO ()) -> IO ()) -> -- Execute a function on the backend.
    StorageIndex ->
    ShareNumbers ->
    NonEmptyList SomeShareData ->
    Property
immutableWriteAndEnumerateShares runBackend storageIndex (ShareNumbers shareNumbers) (NonEmpty shareSeed) = do
    let permutedShares = outerProduct permuteShare shareNumbers (getShareData <$> shareSeed)
        size = sum (fromIntegral . B.length . getShareData <$> shareSeed)
        allocate = AllocateBuckets shareNumbers size
    forAll (traverse jumbleForUpload permutedShares) $ \shareChunks -> monadicIO $ do
        run $
            runBackend $ \backend -> do
                void $ createImmutableStorageIndex backend storageIndex uploadSecret allocate
                writeShares (\sn -> writeImmutableShare backend storageIndex sn uploadSecret) (zip shareNumbers shareChunks)
                readShareNumbers <- getImmutableShareNumbers backend storageIndex
                when (readShareNumbers /= (CBORSet . Set.fromList $ shareNumbers)) $
                    fail (show readShareNumbers ++ " /= " ++ show shareNumbers)
  where
    uploadSecret = Just [anUploadSecret]

jumbleForUpload :: [B.ByteString] -> Gen [(Maybe ByteRanges, B.ByteString)]
jumbleForUpload = shuffle . snd . foldr step (0, [])
  where
    step bs (size, accum) = (size + B.length bs, (Just [ByteRangeFromTo (fromIntegral size) (fromIntegral $ size + B.length bs - 1)], bs) : accum)

-- Immutable share data written to the shares of a given storage index can be
-- retrieved verbatim and associated with the same share numbers as were
-- specified during writing.
immutableWriteAndReadShare ::
    Backend b =>
    ((b -> IO ()) -> IO ()) -> -- Execute a function on the backend.
    StorageIndex ->
    ShareNumbers ->
    NonEmptyList SomeShareData ->
    Property
immutableWriteAndReadShare runBackend storageIndex (ShareNumbers shareNumbers) (NonEmpty shareSeed) = do
    let permutedShares = outerProduct permuteShare shareNumbers (getShareData <$> shareSeed)
        size = sum (fromIntegral . B.length . getShareData <$> shareSeed)
        allocate = AllocateBuckets shareNumbers size
    forAll (traverse jumbleForUpload permutedShares) $ \shareChunks -> monadicIO $ do
        run $
            runBackend $ \backend -> do
                void $ createImmutableStorageIndex backend storageIndex uploadSecret allocate
                writeShares (\sn -> writeImmutableShare backend storageIndex sn uploadSecret) (zip shareNumbers shareChunks)
                readShares' <- mapM (\sn -> readImmutableShare backend storageIndex sn Nothing) shareNumbers
                when (fmap B.concat permutedShares /= readShares') $
                    fail (show permutedShares ++ " /= " ++ show readShares')
  where
    uploadSecret = Just [anUploadSecret]

-- Immutable share data written to the shares of a given storage index cannot
-- be rewritten by a subsequent writeImmutableShare operation.
immutableWriteAndRewriteShare ::
    Backend b =>
    ((b -> IO ()) -> IO ()) -> -- Execute a function on the backend.
    StorageIndex ->
    ShareNumbers ->
    SomeShareData ->
    Property
immutableWriteAndRewriteShare runBackend storageIndex (ShareNumbers shareNumbers) (SomeShareData shareSeed) = monadicIO $ do
    let size = fromIntegral (B.length shareSeed)
        allocate = AllocateBuckets shareNumbers size
        aShareNumber = head shareNumbers
        aShare = permuteShare aShareNumber shareSeed
    run $
        runBackend $ \backend -> do
            void $ createImmutableStorageIndex backend storageIndex uploadSecret allocate
            let write = writeImmutableShare backend storageIndex aShareNumber uploadSecret aShare Nothing
            write
            write `shouldThrow` (== ImmutableShareAlreadyWritten)
  where
    uploadSecret = Just [anUploadSecret]

-- The share numbers of mutable share data written to the shares of a given
-- storage index can be retrieved.
mutableWriteAndEnumerateShares ::
    Backend b =>
    ((b -> IO ()) -> IO ()) -> -- Execute a function on the backend.
    StorageIndex ->
    ShareNumbers ->
    SomeShareData ->
    Property
mutableWriteAndEnumerateShares runBackend storageIndex (ShareNumbers shareNumbers) (SomeShareData shareSeed) = monadicIO $ do
    let permutedShares = flip permuteShare shareSeed <$> shareNumbers
    let nullSecret = WriteEnablerSecret ""
    run $
        runBackend $ \backend -> do
            readvAndTestvAndWritev backend storageIndex nullSecret (mconcat $ zipWith3 writev shareNumbers [0 ..] permutedShares)
                `shouldReturn` ReadTestWriteResult{success = True, readData = mempty}
            (CBORSet readShareNumbers) <- getMutableShareNumbers backend storageIndex
            when (readShareNumbers /= Set.fromList shareNumbers) $
                fail (show readShareNumbers ++ " /= " ++ show shareNumbers)

{- | After an arbitrary number of separate writes complete to construct the
 entire share, any range of the share's bytes can be read.
-}
mutableWriteAndReadShare ::
    Backend b =>
    ((b -> IO ()) -> IO ()) -> -- Execute a function on the backend.
    StorageIndex ->
    B.ByteString ->
    MutableWriteExample ->
    Property
mutableWriteAndReadShare runBackend storageIndex secret MutableWriteExample{..} = monadicIO . run . runBackend $ \backend -> do
    mapM_ (runReadTestWrite_ backend storageIndex (WriteEnablerSecret secret)) (zipWith (writev mweShareNumber) (offsetsFor mweShareData) mweShareData)
    readMutableShare backend storageIndex mweShareNumber mweReadRange `shouldReturn` shareRange
  where
    offsetsFor ranges = scanl (+) 0 $ map (fromIntegral . B.length) ranges

    shareRange :: ShareData
    shareRange = case mweReadRange of
        Nothing -> B.concat mweShareData
        Just ranges -> B.concat $ readRange (B.concat mweShareData) <$> ranges

    readRange shareData (ByteRangeFrom start) = B.drop (fromIntegral start) shareData
    readRange shareData (ByteRangeFromTo start end) = B.take (fromIntegral $ (end - start) + 1) . B.drop (fromIntegral start) $ shareData
    readRange shareData (ByteRangeSuffix len) = B.drop (B.length shareData - fromIntegral len) shareData

withBackend :: Backend b => IO b -> (b -> IO ()) -> (b -> IO ()) -> IO ()
withBackend b cleanup action = do
    backend <- b
    action backend
    cleanup backend

anUploadSecret :: LeaseSecret
anUploadSecret = Upload $ UploadSecret "anuploadsecret"

permuteShare :: ShareNumber -> ShareData -> ShareData
permuteShare (ShareNumber number) = B.map xor'
  where
    xor' :: Word8 -> Word8
    xor' = xor $ fromInteger number

writeShares ::
    (ShareNumber -> shareData -> dataRange -> IO ()) ->
    [(ShareNumber, [(dataRange, shareData)])] ->
    IO ()
writeShares _write [] = return ()
writeShares write ((shareNumber, shareDatav) : rest) = do
    mapM_ (\(range, bs) -> write shareNumber bs range) shareDatav
    writeShares write rest

readVectorToIntervalSet :: [ReadVector] -> IS.IntervalSet Integer
readVectorToIntervalSet rvs = foldr IS.insert IS.empty (f <$> rvs)
  where
    f (ReadVector offset size) = interval (Finite offset, Closed) (Finite $ offset + size, Open)

toFiniteBounds :: Show r => Interval r -> (r, r)
toFiniteBounds i = (lower, upper)
  where
    lower = toFinite (lowerBound i)
    upper = toFinite (upperBound i)

    toFinite n = case n of
        Finite r -> r
        e -> error ("Non-finite bound " <> show e)

writesThatResultIn :: ShareData -> Offset -> Size -> Gen [WriteVector]
writesThatResultIn "" _ _ = pure []
writesThatResultIn bs offset size =
    oneof
        [ -- The whole thing as one write
          pure [WriteVector offset bs]
        , -- Or divide and conquer arbitrarily
          do
            prefixLen <- chooseInteger (0, fromIntegral $ B.length bs)
            pfx <- writesThatResultIn (B.take (fromIntegral prefixLen) bs) offset prefixLen
            sfx <- writesThatResultIn (B.drop (fromIntegral prefixLen) bs) (offset + prefixLen) (size - prefixLen)
            pure $ pfx <> sfx
        , -- Or write some other random somewhere in this range first, to
          -- later be overwritten.
          (:) <$> (WriteVector <$> chooseInteger (offset, offset + size) <*> (chooseInteger (1, size) >>= bytes)) <*> writesThatResultIn bs offset size
        ]

bytes :: Integer -> Gen B.ByteString
bytes len = B.pack <$> vector (fromIntegral len)

extractRead :: Integral a => a -> ShareData -> ReadVector -> ShareData
extractRead lower bs (ReadVector offset size) = B.take (fromIntegral size) . B.drop (fromIntegral offset - fromIntegral lower) $ bs

{- | Define the maximum number of times some "simple" properties will be
 checked.  These are properties where the expectation is that the cardinality
 of the set of paths through the implementation is very small so the cost of
 checking hundreds of different inputs is not worth the benefit.
-}
few :: Int
few = 5

data MutableWriteExample = MutableWriteExample
    { mweShareNumber :: ShareNumber
    , mweShareData :: [ShareData]
    , mweReadRange :: Maybe ByteRanges
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

runReadTestWrite :: Backend b => b -> StorageIndex -> WriteEnablerSecret -> ReadTestWriteVectors -> IO ReadResult
runReadTestWrite backend storageIndex secret rtw = do
    result <- readvAndTestvAndWritev backend storageIndex secret rtw
    if success result then pure (readData result) else throwIO WriteRefused

runReadTestWrite_ :: Backend b => b -> StorageIndex -> WriteEnablerSecret -> ReadTestWriteVectors -> IO ()
runReadTestWrite_ backend storageIndex secret rtw = void $ runReadTestWrite backend storageIndex secret rtw

data WriteRefused = WriteRefused deriving (Show, Eq)
instance Exception WriteRefused
