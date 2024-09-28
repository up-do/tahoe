module SpecZFEC (
    tests,
    prop_encode_decode_roundtrip,
) where

import Test.Tasty (
    TestTree,
    testGroup,
 )

import Test.Tasty.HUnit (
    assertBool,
    testCase,
 )

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.Hedgehog

import Tahoe.CHK (zfec, zunfec)

tests :: TestTree
tests =
    testGroup
        "ZFEC"
        [ testProperty "data round-trips through encode/decode" prop_encode_decode_roundtrip
        , test_encode
        , test_decode
        ]

test_encode :: TestTree
test_encode = testCase "ZFEC.encode" $ do
    let k = 3
        n = 10
        x = 131073
        plaintext = BS.replicate x 73
    encoded <- liftIO $ zfec k n plaintext
    assertBool "It encoded" (and $ (x `div` k ==) . BS.length <$> encoded)

test_decode :: TestTree
test_decode = testCase "ZFEC.decode" $ do
    let k = 3
        n = 10
        x = 131073 `div` k
        encoded = replicate k (BS.replicate x 31)
    plaintext <- liftIO $ zunfec k n (zip [0 ..] encoded)
    assertBool "It decoded" $ BS.length plaintext == 131073

{- | Given any `k` output blocks from @'ZFEC.encode' k n plaintext@, @'decode'
 k n@ will reproduce the @plaintext@ input.
-}
prop_encode_decode_roundtrip :: Property
prop_encode_decode_roundtrip = property $ do
    k <- forAll $ Gen.int (Range.linear 1 254)
    n <- forAll $ (k +) <$> Gen.int (Range.linear 1 (255 - k))

    -- Ensure the plaintext has an allowed length.
    plaintextLength <- forAll $ (k *) <$> Gen.integral (Range.linear 1 32)
    plaintext <- forAll $ Gen.bytes (Range.singleton plaintextLength)

    encodedBlocks <- evalIO $ zfec k n plaintext
    let tagged = zip [0 ..] encodedBlocks
    annotateShow encodedBlocks
    available <- forAll $ Gen.shuffle tagged

    decodedPlaintext <- evalIO $ zunfec k n (take k available)

    diff plaintext (==) decodedPlaintext
