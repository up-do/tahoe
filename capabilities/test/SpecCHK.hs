{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SpecCHK (
    tests,
    testsFromVectors,
) where

import Control.Arrow (
    (&&&),
 )
import Control.Lens (view)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Crypto.Cipher.AES (AES128)
import qualified Data.Binary as Binary
import Data.ByteArray (convert)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as BL
import Data.Coerce (coerce)
import Data.Either (isLeft)
import Data.Text (
    concat,
    unpack,
 )
import Data.TreeDiff.Class (ToExpr, ediff)
import Data.TreeDiff.Pretty (prettyEditExpr)
import GHC.Generics (Generic)
import Generators (
    applyShareBitFlips,
    blockBitFlipper,
    crypttextTreeLeafBitFlipper,
    digests,
    fingerprintBitFlipper,
    genParameters,
    shareTreeLeafBitFlipper,
    shares,
 )
import Hedgehog (
    Property,
    annotateShow,
    assert,
    diff,
    forAll,
    property,
    tripping,
 )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Tahoe.CHK (padCiphertext)
import qualified Tahoe.CHK (decode, encode, segmentCiphertext)
import Tahoe.CHK.Capability (Reader, dangerRealShow, pCapability, pReader, verifier)
import Tahoe.CHK.Cipher (Key)
import Tahoe.CHK.Crypto (ciphertextSegmentHash', convergenceSecretLength)
import Tahoe.CHK.Encrypt (encryptLazy)
import Tahoe.CHK.Share (
    Share (
        _blockSize
    ),
    uriExtension,
 )
import Tahoe.CHK.Types (
    Parameters (..),
 )
import Tahoe.CHK.URIExtension (codecParams)
import Tahoe.CHK.Upload (
    UploadResult (..),
    Uploadable (..),
    adjustSegmentSize,
    getConvergentKey,
    memoryUploadableWithConvergence,
    store,
 )
import Tahoe.CHK.Validate (
    matchingBlockHashRoot,
    validFingerprint,
    validSegment,
    validShareRootHash,
 )
import Tahoe.Server (
    nullStorageServer,
 )
import Test.Tasty (
    TestTree,
    testGroup,
 )
import Test.Tasty.HUnit (
    Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
 )
import Test.Tasty.Hedgehog (testProperty)
import Text.Megaparsec (parse)
import Vectors (
    Format (..),
    JSONByteString (..),
    Sample (..),
    TestCase (..),
    VectorSpec (..),
    WellKnown (..),
    loadWellKnownCase,
 )

{- | Encrypt and encode some plaintext using some parameters, producing some
   shares and a read capability.

   A hard-coded convergence secret is used for simplicity and reproducibility.
-}
makeValidShares :: Parameters -> BL.ByteString -> IO ([Share], Reader)
makeValidShares params plaintext = Tahoe.CHK.encode key params (encryptLazy key plaintext)
  where
    key = getConvergentKey "secret" params plaintext

assertEqual' :: (Generic a, ToExpr a, Eq a) => a -> a -> Assertion
assertEqual' a b = assertBool (show . prettyEditExpr $ ediff a b) (a == b)

-- | Create tests for each case in the test vector specification.
testsFromVectors :: VectorSpec -> TestTree
testsFromVectors vectorSpec =
    testGroup
        "Vectors"
        [ testCap vectorSpec
        , testCapabilityParser vectorSpec
        ]

tests :: TestTree
tests =
    testGroup
        "CHK"
        [ testEncrypt
        , testProperty "expand returns the correct number of bytes" prop_expand_length
        , testProperty "expand returns bytes containing the template repeated" prop_expand_template
        , testProperty "Share round-trips through put / get" prop_share_roundtrip
        , testWellKnownShare1of2
        , testWellKnownShare2of3
        , testWellKnownShare3of10
        , testProperty "segmentCiphertext preserves all of the ciphertext" prop_segmentCiphertext_identity
        , testProperty "padCiphertext returns a string with a length that is a multiple of the given requiredShares value" prop_paddedCiphertext_boundary
        , testProperty "ciphertext round-trips through decode . encode" prop_share_encoding_roundtrip
        , testSizes
        , testOutOfBoundsShareNumbers
        , testProperty "decode signals error if the integrity of the shares is compromised" propIntegrity
        , testProperty "validSegment returns False if called with a hash not related to a ciphertext by the ciphertext segment hash function" propInvalidSegment
        ]

data Described descr b = Described descr b

instance Show descr => Show (Described descr b) where
    show (Described descr _) = show descr

{- | Tahoe.CHK.decode returns a Left value if the capability fingerprint does
 not equal the URI extension block hash for any share to be decoded.
-}
propIntegrity :: Property
propIntegrity = property $ do
    -- First synthesize some intact shares and the associated read capability.
    plaintext <- forAll $ BL.fromStrict <$> Gen.bytes (Range.linear 1 1024)
    params <- forAll $ fixParams <$> genParameters

    valid@(validShares, validCap) <- liftIO $ makeValidShares params plaintext
    annotateShow validShares
    annotateShow validCap

    -- Pick a function to use to screw them up somehow.  Wrap them in
    -- something Showable for the sake of `forAll`.
    let mungers =
            [ fingerprintBitFlipper validCap
            , shareTreeLeafBitFlipper params validShares
            , blockBitFlipper params validShares
            , crypttextTreeLeafBitFlipper params validShares
            ]
    munge <- forAll $ Gen.choice mungers

    -- Verify that decoding with the screwed up values signals a lack of
    -- integrity.
    let munged@(mungedShares, mungedCap) = applyShareBitFlips munge valid
    annotateShow mungedShares
    annotateShow mungedCap

    -- Sanity check - something must have changed or the decode _should_
    -- succeed (not what we want to test here).
    diff valid (/=) munged

    -- Show us the difference
    annotateShow $ prettyEditExpr (ediff valid munged)

    let taggedShares = zip [0 ..] mungedShares
    shuffledShares <- forAll $ Gen.shuffle taggedShares
    result <- liftIO $ Tahoe.CHK.decode mungedCap shuffledShares
    diff True (==) (isLeft result)

testSizes :: TestTree
testSizes =
    testCase "the maximum segment size encoded in the UEB equals the actual segment size" $ do
        uploadable <- memoryUploadableWithConvergence (B.replicate 32 0x00) (fromIntegral $ BL.length ciphertext) ciphertext params
        (shares', _cap) <- Tahoe.CHK.encode (uploadableKey uploadable) params ciphertext
        mapM_ (assertEqual "The shareBlockSize reflects the parameters and real ciphertext size" (fromIntegral $ BL.length ciphertext `div` 2) . _blockSize) shares'
        mapM_ (assertEqual "The segment size is reduced to the ciphertext size" (fromIntegral $ BL.length ciphertext) . getSegmentSize) shares'
  where
    getSegmentSize = paramSegmentSize . view (uriExtension . codecParams)
    params =
        Parameters
            { paramSegmentSize = 100000
            , paramTotalShares = 3
            , paramHappyShares = 1
            , paramRequiredShares = 2
            }
    ciphertext = BL.pack [1 .. 56]

{- | segmentCiphertext may split up ciphertext but it may not change its
 content in any way.
-}
prop_segmentCiphertext_identity :: Property
prop_segmentCiphertext_identity = property $ do
    ciphertext <- forAll $ BL.fromStrict <$> Gen.bytes (Range.linear 1 1024)
    params <- forAll genParameters

    let segments = Tahoe.CHK.segmentCiphertext params ciphertext
        recovered = BL.concat segments

    diff ciphertext (==) recovered

prop_paddedCiphertext_boundary :: Property
prop_paddedCiphertext_boundary = property $ do
    ciphertext <- forAll $ BL.fromStrict <$> Gen.bytes (Range.linear 1 1024)
    Parameters{paramRequiredShares} <- forAll genParameters

    let padded = padCiphertext paramRequiredShares ciphertext

    diff (BL.length padded `mod` fromIntegral paramRequiredShares) (==) 0

{- | Assert that:

 * shares of a certain well-known case can be decoded and re-encoded to the same byte sequences
 * we can create those same shares and the corresponding capability by re-encoding the same inputs
 * we can validate the UEB fingerprint for each share
 * we can validate the share tree root hash included in each share
-}
wellKnownCase :: WellKnown -> Assertion
wellKnownCase WellKnown{..} =
    do
        uploadable <- memoryUploadableWithConvergence wellKnownConvergenceSecret (fromIntegral $ BL.length wellKnownPlaintext) wellKnownPlaintext wellKnownParameters
        let ciphertext = encryptLazy (uploadableKey uploadable) wellKnownPlaintext
        (shares', cap) <- Tahoe.CHK.encode (uploadableKey uploadable) wellKnownParameters ciphertext

        let allValid = replicate (fromIntegral $ paramTotalShares wellKnownParameters) True

        let expectedShares = Binary.decode <$> wellKnownShares
            encodedShares = Binary.encode <$> shares'

        assertEqual' expectedShares shares'
        assertEqual' wellKnownShares encodedShares
        assertEqual "The cap matches" cap wellKnownCapability
        assertEqual "The fingerprint matches" allValid ((validFingerprint . view verifier $ wellKnownCapability) <$> expectedShares)
        assertEqual "The block tree root hash matches the proof" allValid (zipWith matchingBlockHashRoot [0 ..] expectedShares)
        assertEqual "The share tree root hash is consistent" allValid (validShareRootHash $ zip [0 ..] expectedShares)
        pure ()

testWellKnownShare1of2 :: TestTree
testWellKnownShare1of2 = testCase "a known 1-of-2 case encodes as expected" (loadWellKnownCase params cap >>= wellKnownCase)
  where
    params =
        Parameters
            { paramSegmentSize = 8
            , paramHappyShares = 1
            , paramRequiredShares = 1
            , paramTotalShares = 2
            }
    cap = "URI:CHK:pyv3qypbpk6knq5ozeibenuubq:jh3twlgmxtytwqtzn6jtbsfy2w574ybkcnalurlnlq2snuu3j5da:1:2:56"

testWellKnownShare2of3 :: TestTree
testWellKnownShare2of3 = testCase "a known 2-of-3 case encodes as expected" (loadWellKnownCase params cap >>= wellKnownCase)
  where
    params =
        Parameters
            { paramSegmentSize = 8
            , paramHappyShares = 1
            , paramRequiredShares = 2
            , paramTotalShares = 3
            }
    cap = "URI:CHK:co4s2wzrwos726nu24ervz2ffu:orrq3znudwnwgcazuc7qbm3prf4a46c3gmboecbror4l2k62jtkq:2:3:56"

testWellKnownShare3of10 :: TestTree
testWellKnownShare3of10 = testCase "a known 3-of-10 case encodes as expected" (loadWellKnownCase params cap >>= wellKnownCase)
  where
    params =
        Parameters
            { paramSegmentSize = 8
            , paramHappyShares = 1
            , paramRequiredShares = 3
            , paramTotalShares = 10
            }
    cap = "URI:CHK:o4lpfdvt7ib5xei2qhz6ovkz34:uvhgccbgigj4gfqfeyh5g5uogyt7etmlmqnvswqxumm7q3rqh7uq:3:10:56"

prop_share_encoding_roundtrip :: Property
prop_share_encoding_roundtrip = property $ do
    convergenceSecret <- forAll $ Gen.bytes (Range.singleton 32)
    ciphertext <- forAll $ BL.fromStrict <$> Gen.bytes (Range.linear 1 2048)
    params <- forAll $ fixParams <$> genParameters
    let key = getConvergentKey convergenceSecret (adjustSegmentSize params (fromIntegral $ BL.length ciphertext)) ciphertext
    (shares', cap) <- liftIO $ Tahoe.CHK.encode key params ciphertext
    recovered <- liftIO $ Tahoe.CHK.decode cap (zip [0 ..] shares')

    diff (Right ciphertext) (==) recovered

-- XXX Our ZFEC bindings are unhappy with k == n.  genParameters will
-- happily give us that so adjust k or n if we happen to hit such a case.
fixParams :: Parameters -> Parameters
fixParams p@Parameters{paramRequiredShares = 256, paramTotalShares = 256} = p{paramRequiredShares = 255}
fixParams p@Parameters{paramRequiredShares, paramTotalShares}
    | paramRequiredShares == paramRequiredShares = p{paramTotalShares = paramTotalShares + 1}
    | otherwise = p

prop_share_roundtrip :: Property
prop_share_roundtrip =
    let decode' = ((\(_, _, sh) -> sh) <$>) . Binary.decodeOrFail
     in property $ do
            share <- forAll shares
            tripping share Binary.encode decode'

testEncrypt :: TestTree
testEncrypt =
    testGroup
        "chkEncrypt"
        [ testCase "ciphertext" $ do
            assertEqual
                "expected convergence key"
                "oBcuR/wKdCgCV2GKKXqiNg=="
                (Base64.encode $ convert convergenceKey)
            let b64ciphertext = Base64.encode (BL.toStrict ciphertext)
            assertEqual "known result" knownCorrect b64ciphertext
        ]
  where
    -- For all the magic values see
    -- allmydata.test.test_upload.FileHandleTests.test_get_encryption_key_convergent
    knownCorrect :: B.ByteString
    knownCorrect = "Jd2LHCRXozwrEJc="

    plaintext :: BL.ByteString
    plaintext = "hello world"

    ciphertext :: BL.ByteString
    ciphertext = encryptLazy convergenceKey plaintext

    convergenceKey :: Key AES128
    convergenceKey = getConvergentKey convergenceSecret params plaintext

    convergenceSecret = B.replicate convergenceSecretLength 0x42
    params =
        adjustSegmentSize
            Parameters
                { paramSegmentSize = 128 * 1024
                , paramTotalShares = 10
                , paramHappyShares = 5
                , paramRequiredShares = 3
                }
            (fromIntegral $ BL.length plaintext)

{- | Build a test tree that applies a test function to every CHK case in a
 test vector.
-}
chkTests ::
    -- | A name to give the group of tests.
    String ->
    -- | A function to call with a CHK test case to get back a test.
    (TestCase -> Assertion) ->
    -- | The test vector containing CHK test cases.
    VectorSpec ->
    -- | A test tree with one test per CHK case in the test vector.
    TestTree
chkTests name makeOneTest =
    testGroup name . map (uncurry ($) . (testCase . unpack . expected &&& makeOneTest)) . filter pickCase . vector
  where
    pickCase TestCase{format, zfec} = format == CHK && (paramTotalShares zfec > paramRequiredShares zfec && paramTotalShares zfec < 256)

{- | Every CHK case in the test vector can be reproduced by this
 implementation.
-}
testCap :: VectorSpec -> TestTree
testCap = chkTests "chkCap" testOneCase

{- | Every CHK capability in the test vector can be parsed and then serialized
 back to the same byte string.
-}
testCapabilityParser :: VectorSpec -> TestTree
testCapabilityParser = chkTests "testCapabilityParser" testParseOneCapability

{- | Assert that a specific CHK capability can be parsed and serialized back
 to the same byte string.
-}
testParseOneCapability :: TestCase -> Assertion
testParseOneCapability TestCase{expected} = do
    serialized <- case parse pCapability "" expected of
        Left err -> assertFailure $ show err
        Right cap -> pure $ dangerRealShow cap
    assertEqual "expected /= serialized" expected serialized

{- | Assert that verify and read capability strings with n/k/size below the
 minimum legal or above the maximum legal value are rejected by the parser.
-}
testOutOfBoundsShareNumbers :: TestTree
testOutOfBoundsShareNumbers =
    testCase
        "out-of-bounds share numbers cause capability string parse errors"
        $ mapM_ assertParseFail cases
  where
    cases =
        [ -- Verify caps with n/k/size too small
          "URI:CHK-Verifier:yzxcoagbetwet65ltjpbqyli3m:6b7inuiha2xdtgqzd55i6aeggutnxzr6qfwpv2ep5xlln6pgef7a:0:1:56"
        , "URI:CHK-Verifier:yzxcoagbetwet65ltjpbqyli3m:6b7inuiha2xdtgqzd55i6aeggutnxzr6qfwpv2ep5xlln6pgef7a:1:0:56"
        , "URI:CHK-Verifier:yzxcoagbetwet65ltjpbqyli3m:6b7inuiha2xdtgqzd55i6aeggutnxzr6qfwpv2ep5xlln6pgef7a:1:1:0"
        , -- Read caps with n/k/size too small
          "URI:CHK:yzxcoagbetwet65ltjpbqyli3m:6b7inuiha2xdtgqzd55i6aeggutnxzr6qfwpv2ep5xlln6pgef7a:0:1:56"
        , "URI:CHK:yzxcoagbetwet65ltjpbqyli3m:6b7inuiha2xdtgqzd55i6aeggutnxzr6qfwpv2ep5xlln6pgef7a:1:0:56"
        , "URI:CHK:yzxcoagbetwet65ltjpbqyli3m:6b7inuiha2xdtgqzd55i6aeggutnxzr6qfwpv2ep5xlln6pgef7a:1:1:0"
        , -- Verify caps with n/k/size too large
          "URI:CHK-Verifier:yzxcoagbetwet65ltjpbqyli3m:6b7inuiha2xdtgqzd55i6aeggutnxzr6qfwpv2ep5xlln6pgef7a:257:256:1000"
        , "URI:CHK-Verifier:yzxcoagbetwet65ltjpbqyli3m:6b7inuiha2xdtgqzd55i6aeggutnxzr6qfwpv2ep5xlln6pgef7a:256:257:1000"
        , "URI:CHK-Verifier:yzxcoagbetwet65ltjpbqyli3m:6b7inuiha2xdtgqzd55i6aeggutnxzr6qfwpv2ep5xlln6pgef7a:256:256:18446744073709551616"
        , -- Read caps with n/k/size too large
          "URI:CHK:yzxcoagbetwet65ltjpbqyli3m:6b7inuiha2xdtgqzd55i6aeggutnxzr6qfwpv2ep5xlln6pgef7a:257:256:1000"
        , "URI:CHK:yzxcoagbetwet65ltjpbqyli3m:6b7inuiha2xdtgqzd55i6aeggutnxzr6qfwpv2ep5xlln6pgef7a:256:257:1000"
        , "URI:CHK:yzxcoagbetwet65ltjpbqyli3m:6b7inuiha2xdtgqzd55i6aeggutnxzr6qfwpv2ep5xlln6pgef7a:256:256:18446744073709551616"
        ]

    assertParseFail s =
        case parse pCapability "" s of
            Left _err -> pure ()
            Right cap ->
                assertFailure . unpack . Data.Text.concat $
                    [ "Expected parse failure of "
                    , s
                    , " instead got "
                    , dangerRealShow cap
                    ]

{- | Assert that a specific CHK case can be reproduced by this implementation.
 This means we can encode the same plaintext using the same secrets to the
 same ciphertext and share layout and that the resulting capability string
 is the same byte sequence as given by the test vector.
-}
testOneCase :: TestCase -> Assertion
testOneCase
    TestCase
        { convergence
        , format = CHK
        , sample
        , zfec
        , expected
        } =
        do
            uploadable <- memoryUploadableWithConvergence (coerce convergence) (fromIntegral $ sampleLength sample) (BL.fromStrict $ expand sample) zfec
            upresult <- store [nullStorageServer] uploadable
            assertEqual "yes" (parse pReader "" expected) (Right $ uploadResultReadCap upresult)
testOneCase x = error $ "testOneCase got bad input" <> show x

expand :: Sample -> B.ByteString
expand (Sample sampleTemplate sampleLength) =
    B.take sampleLength . B.concat $ take sampleLength (replicate n bs)
  where
    n = (sampleLength `div` B.length bs) + 1
    bs = coerce sampleTemplate -- yuck

prop_expand_length :: Property
prop_expand_length =
    property $ do
        sample <- forAll $ Sample <$> (JSONByteString <$> Gen.bytes (Range.linear 1 16)) <*> Gen.int (Range.linear 1 1000)
        diff (sampleLength sample) (==) (B.length $ expand sample)

prop_expand_template :: Property
prop_expand_template =
    property $ do
        template <- forAll $ Gen.bytes (Range.linear 1 16)
        sample <- forAll $ Sample (JSONByteString template) <$> Gen.int (Range.linear 1 1000)
        assert $ checkTemplate template (expand sample)
  where
    checkTemplate :: B.ByteString -> B.ByteString -> Bool
    checkTemplate _ "" = True
    checkTemplate template expanded =
        all (uncurry (==)) (B.zip template expanded)
            && checkTemplate template (B.drop (B.length template) expanded)

propInvalidSegment :: Property
propInvalidSegment = property $ do
    ciphertext <- forAll $ Gen.bytes (Range.linear 1 64)
    expected <- forAll $ Gen.filterT (ciphertextSegmentHash' ciphertext /=) digests
    diff Nothing (==) (validSegment expected ciphertext)
