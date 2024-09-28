module SpecSSK where

import Hedgehog (
    annotateShow,
    diff,
    forAll,
    property,
    tripping,
 )

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Crypto.Cipher.Types (makeIV)
import Crypto.Hash (digestFromByteString)
import Data.ASN1.BinaryEncoding (DER (DER))
import Data.ASN1.Encoding (decodeASN1')
import qualified Data.Binary as Binary
import Data.Binary.Get (ByteOffset)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as B
import Data.ByteString.Base32 (decodeBase32Unpadded, encodeBase32Unpadded)
import qualified Data.ByteString.Lazy as LB
import Data.Either (rights)
import qualified Data.Text as T
import GeneratorsSSK (capabilities, encodingParameters, genRSAKeys, ivLength, shareHashChains, shares)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.IO (hSetEncoding, stderr, stdout, utf8)
import Tahoe.CHK.SHA256d (Digest' (Digest'))
import Tahoe.Capability (confidentiallyShow)
import qualified Tahoe.SDMF
import Tahoe.SDMF.Internal.Capability (deriveVerifier)
import Tahoe.SDMF.Internal.Keys (signatureKeyFromBytes, signatureKeyToBytes)
import qualified Tahoe.SDMF.Keys as Keys
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Hedgehog (testProperty)
import Text.Megaparsec (parse)

-- The test suite compares against some hard-coded opaque strings.  These
-- expected values were determined using the expected_values.py program in
-- this directory.

tests :: TestTree
tests =
    testGroup
        "SSK"
        [ testProperty "Hash chain round-trips through bytes" $
            property $ do
                hashChain <- forAll shareHashChains
                tripping hashChain Binary.encode decode'
        , testProperty "Signatures round-trip through signatureKeyToBytes . signatureKeyFromBytes" $
            property $ do
                key <- forAll genRSAKeys
                tripping (Keys.Signature . Keys.toPrivateKey $ key) signatureKeyToBytes signatureKeyFromBytes
        , testCase "Signature byte-serializations round-trip through signatureKeyFromBytes . signatureKeyToBytes" $ do
            let keyPaths =
                    [ -- Check ours
                      "test/data/rsa-privkey-0.der"
                    , "test/data/rsa-privkey-1.der"
                    , "test/data/rsa-privkey-2.der"
                    , "test/data/rsa-privkey-3.der"
                    , "test/data/rsa-privkey-4.der"
                    , -- And one from Tahoe-LAFS
                      "test/data/tahoe-lafs-generated-rsa-privkey.der"
                    ]
                checkSignatureRoundTrip p =
                    B.readFile p >>= \original ->
                        let (Right sigKey) = signatureKeyFromBytes original
                            serialized = signatureKeyToBytes sigKey
                         in do
                                -- They should decode to the same structure.  This
                                -- has the advantage of representing differences a
                                -- little more transparently than the next
                                -- assertion.
                                assertEqual
                                    "decodeASN1 original /= decodeASN1 serialized"
                                    (decodeASN1' DER original)
                                    (decodeASN1' DER serialized)
                                -- Also check the raw bytes in case there
                                -- are different representations of the
                                -- structure possible.  The raw bytes
                                -- matter because we hash them in key
                                -- derivations.
                                assertEqual "original /= serialized" original serialized
            -- Check them all
            mapM_ checkSignatureRoundTrip keyPaths
        , testCase "derived keys equal known-correct values" $
            -- The path is relative to the root of the package, which is where
            -- at least some test runners will run the test process.  If
            B.readFile "test/data/rsa-privkey-0.der" >>= \privBytes ->
                let -- Load the test key.
                    (Right sigKey) = signatureKeyFromBytes privBytes

                    -- Hard-code the expected result.
                    expectedWriteKey = ("v7iymuxkc5yv2fomi3xwbjdd4e" :: T.Text)
                    expectedReadKey = ("6ir6husgx6ubro3tbimmzskqri" :: T.Text)
                    expectedDataKey = ("bbj67exlrkfcaqutwlgwvukbfe" :: T.Text)
                    expectedStorageIndex = ("cmkuloz2t6fhsh7npxxteba6sq" :: T.Text)
                    expectedWriteEnablerMaster = ("qgptod5dsanfep2kbimvxl2yixndnoks7ndoeamczj7g33gokcvq" :: T.Text)
                    expectedWriteEnabler = ("bg4ldrgfyiffufltcuttr3cnrmrjfpoxc65qdoqa6d5izkzofl5q" :: T.Text)

                    -- Constants involved in the derivation.  These agree with
                    -- those used to generate the above expected values.
                    (Just iv) = Keys.SDMF_IV <$> makeIV (B.replicate 16 0x42)
                    peerid = B.replicate 20 0x42

                    -- Derive all the keys.
                    (Just w@(Keys.Write _ derivedWriteKey)) = Keys.deriveWriteKey sigKey
                    (Just r@(Keys.Read _ derivedReadKey)) = Keys.deriveReadKey w
                    (Just (Keys.Data _ derivedDataKey)) = Keys.deriveDataKey iv r
                    (Keys.StorageIndex derivedStorageIndex) = Keys.deriveStorageIndex r
                    wem@(Keys.WriteEnablerMaster derivedWriteEnablerMaster) = Keys.deriveWriteEnablerMaster w
                    (Keys.WriteEnabler derivedWriteEnabler) = Keys.deriveWriteEnabler peerid wem
                    -- A helper to format a key as text for convenient
                    -- comparison to expected value.
                    fmtKey = T.toLower . encodeBase32Unpadded . ByteArray.convert
                 in do
                        -- In general it might make more sense to convert expected
                        -- into ScrubbedBytes instead of converting derived into
                        -- ByteString but ScrubbedBytes doesn't have a useful Show
                        -- instance so we go the other way.  We're not worried about
                        -- the safety of these test-only keys anyway.
                        assertEqual
                            "writekey: expected /= derived"
                            expectedWriteKey
                            (fmtKey derivedWriteKey)
                        assertEqual
                            "readkey: expected /= derived"
                            expectedReadKey
                            (fmtKey derivedReadKey)
                        assertEqual
                            "datakey: expected /= derived"
                            expectedDataKey
                            (fmtKey derivedDataKey)
                        assertEqual
                            "storage index: expected /= derived"
                            expectedStorageIndex
                            (T.toLower . encodeBase32Unpadded $ derivedStorageIndex)
                        assertEqual
                            "write enabler master: expected /= derived"
                            expectedWriteEnablerMaster
                            (fmtKey derivedWriteEnablerMaster)
                        assertEqual
                            "write enabler: expected /= derived"
                            expectedWriteEnabler
                            (fmtKey derivedWriteEnabler)
        , testCase "known-correct SDMF capability strings round-trip through parse . confidentiallyShow" $ do
            let validWrite = "URI:SSK:vbopclzrkxces6okoqfarapmou:xlwog3jxbgsuaddh3bsofwmyhncv7fanmo7ujhqiy26usx2v2neq"
                validRead = "URI:SSK-RO:ro7pnpq6duaduuolookwbv5lqy:xlwog3jxbgsuaddh3bsofwmyhncv7fanmo7ujhqiy26usx2v2neq"
                validVerify = "URI:SSK-Verifier:gz4s2zkkqy2geblvv77atyoppi:xlwog3jxbgsuaddh3bsofwmyhncv7fanmo7ujhqiy26usx2v2neq"

                parsed = rights $ parse Tahoe.SDMF.pCapability "<test>" <$> [validWrite, validRead, validVerify]
                serialized = confidentiallyShow <$> parsed

            assertEqual "parsing failed" 3 (length parsed)
            assertEqual "original /= serialized" [validWrite, validRead, validVerify] serialized

            let [Tahoe.SDMF.SDMFWriter writeCap, Tahoe.SDMF.SDMFReader readCap, Tahoe.SDMF.SDMFVerifier verifyCap] = parsed
                derivedReader = Tahoe.SDMF.writerReader writeCap
                derivedVerifier = Tahoe.SDMF.readerVerifier readCap

            assertEqual "derived reader /= parsed reader" derivedReader readCap
            assertEqual "serialized derived reader /= original" (confidentiallyShow derivedReader) validRead
            assertEqual "derived verifier /= parsed verifier" derivedVerifier verifyCap
            assertEqual "serialized derived verifier /= original" (confidentiallyShow derivedVerifier) validVerify
        , testProperty "SDMF capabilities round-trip through confidentiallyShow . parse pCapability" $
            property $ do
                cap <- forAll capabilities
                tripping cap confidentiallyShow (parse Tahoe.SDMF.pCapability "<text>")
        , testProperty "Share round-trips through bytes" $
            property $ do
                share <- forAll shares
                tripping share Binary.encode decode'
        , testCase "known-correct serialized shares round-trip though Share" $
            mapM_ knownCorrectRoundTrip [0 :: Int .. 9]
        , testProperty "Ciphertext round-trips through encode . decode" $
            property $ do
                keypair <- forAll genRSAKeys
                ivBytes <- forAll $ Gen.bytes (Range.singleton ivLength)
                let Just iv = Keys.SDMF_IV <$> makeIV ivBytes
                ciphertext <- forAll $ LB.fromStrict <$> Gen.bytes (Range.exponential 1 1024)
                sequenceNumber <- forAll $ Gen.integral Range.exponentialBounded
                (required, total) <- forAll encodingParameters

                (shares', Tahoe.SDMF.Writer{Tahoe.SDMF.writerReader}) <- liftIO $ Tahoe.SDMF.encode keypair iv sequenceNumber required total ciphertext

                annotateShow shares'

                recovered <- Tahoe.SDMF.decode writerReader (zip [0 ..] shares')
                diff ciphertext (==) recovered
        , testProperty "Plaintext round-trips through encrypt . decrypt" $
            property $
                do
                    keypair <- forAll genRSAKeys
                    (Just iv) <- fmap Keys.SDMF_IV <$> (makeIV <$> forAll (Gen.bytes (Range.singleton 16)))
                    let (Just readKey) = do
                            writeKey <- Keys.deriveWriteKey (Keys.toSignatureKey keypair)
                            Keys.deriveReadKey writeKey
                    plaintext <- forAll $ LB.fromStrict <$> Gen.bytes (Range.exponential 1 1024)
                    tripping plaintext (Tahoe.SDMF.encrypt keypair iv) (Just . Tahoe.SDMF.decrypt readKey iv)
        , testCase "Recover plaintext from a known-correct slot" $ do
            s0 <- liftIO $ Binary.decode <$> (LB.readFile "test/data/3of10.0" >>= readShareFromBucket)
            s6 <- liftIO $ Binary.decode <$> (LB.readFile "test/data/3of10.6" >>= readShareFromBucket)
            s9 <- liftIO $ Binary.decode <$> (LB.readFile "test/data/3of10.9" >>= readShareFromBucket)

            let (Right writeKey) = Binary.decode . LB.fromStrict <$> decodeBase32Unpadded "vdv6pcqkblsguvkagrblr3gopu"
                (Just readerReadKey) = Keys.deriveReadKey writeKey
                (Just readerVerifier) = deriveVerifier readerReadKey . Digest' <$> digestFromByteString ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" :: B.ByteString)
                reader = Tahoe.SDMF.Reader{..}
            ciphertext <- Tahoe.SDMF.decode reader [(0, s0), (6, s6), (9, s9)]
            let (Right expectedCiphertext) = LB.fromStrict <$> decodeBase32Unpadded "6gutkha6qd4g3lxahth2dw2wjekadwoxvmazrnfq5u5j6a7quu5qy6nz3dvosx2gisdjshdtd5xphqvqjco5pq73qi"
                (Right (Just expectedIV)) = fmap (fmap Keys.SDMF_IV . makeIV) . decodeBase32Unpadded $ "xkczackg4djsvtx5brgy4z3pse"
                (Right expectedReadKey) = Binary.decode . LB.fromStrict <$> decodeBase32Unpadded "g4fimjxgdpwrvpfguyz5a6hvz4"
                (Right expectedDataKey) = Binary.decode . LB.fromStrict <$> decodeBase32Unpadded "crblibtnjacos5xwjpxb2d5hla"
                expectedPlaintext = "abcdefghijklmnopqrstuvwxyzZYXWVUTSRQPONMLKJIJHGRFCBA1357"

                (Just dataKey) = Keys.deriveDataKey (Tahoe.SDMF.shareIV s0) readerReadKey
                recoveredPlaintext = Tahoe.SDMF.decrypt readerReadKey (Tahoe.SDMF.shareIV s0) ciphertext

            assertEqual "read key: expected /= derived" expectedReadKey readerReadKey
            assertEqual "data key: expected /= derived" expectedDataKey dataKey
            assertEqual "iv: expected /= loaded" expectedIV (Tahoe.SDMF.shareIV s0)
            assertEqual "ciphertext: expected /= decoded" expectedCiphertext ciphertext

            assertEqual "expected /= recovered" expectedPlaintext recoveredPlaintext
        ]

readShareFromBucket :: MonadFail m => LB.ByteString -> m LB.ByteString
readShareFromBucket bucket =
    let withoutPrefix = LB.drop (32 + 20 + 32 + 8 + 8 + 368) bucket
        dataSize = LB.length withoutPrefix - 4
        shareData = LB.take dataSize withoutPrefix
        suffix = LB.drop dataSize withoutPrefix
     in do
            when (suffix /= "\0\0\0\0") (fail "Cannot account for extra leases")
            pure shareData

{- | Load a known-correct SDMF bucket and assert that bytes in the slot it
 contains deserializes to a Share and then serializes back to the same bytes

 Note: The capability for the test data is:

   URI:SSK:vdv6pcqkblsguvkagrblr3gopu:6pd5r2qrsb3zuq2n6ocvcsg2a6b47ehclqxidkzd5awdabhtdo6a
-}
knownCorrectRoundTrip :: Show a => a -> IO ()
knownCorrectRoundTrip n = do
    -- The files are in "bucket" format.  We need to extract the
    -- "slot".  We do so by stripping a prefix and suffix.  To avoid
    -- having to parse the prefix, we assert that the suffix is a
    -- predictable size.
    bucket <- LB.readFile ("test/data/3of10." <> show n)
    shareData <- readShareFromBucket bucket

    let decoded = decode' shareData
    let encoded = (Binary.encode :: Tahoe.SDMF.Share -> LB.ByteString) <$> decoded
    assertEqual "original /= encoded" (Right shareData) encoded

    -- We also know some specific things about the know-correct shares.
    let (Right sh) = decoded
    assertEqual "3 /= required" 3 (Tahoe.SDMF.shareRequiredShares sh)
    assertEqual "10 /= total" 10 (Tahoe.SDMF.shareTotalShares sh)

-- | Like `Binary.Binary.decodeOrFail` but only return the decoded value.
decode' :: Binary.Binary b => LB.ByteString -> Either (LB.ByteString, ByteOffset, String) b
decode' = ((\(_, _, a) -> a) <$>) . Binary.decodeOrFail

main :: IO ()
main = do
    -- Hedgehog writes some non-ASCII and the whole test process will die if
    -- it can't be encoded.  Increase the chances that all of the output can
    -- be encoded by forcing the use of UTF-8 (overriding the LANG-based
    -- choice normally made).
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    defaultMain tests
