{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module SpecCrypto (
    tests,
) where

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (Cipher (..), KeySizeSpecifier (..))
import Crypto.Error (CryptoFailable (CryptoPassed))
import qualified Data.ByteString as B
import Data.Char (
    ord,
 )
import Tahoe.CHK.Cipher (Key)
import Tahoe.CHK.Crypto (
    blockHash',
    ciphertextSegmentHash',
    convergenceEncryptionTag,
    convergenceSecretLength,
    sha256,
    sha256d,
    storageIndexHash,
    taggedHash,
    taggedHash',
    taggedPairHash,
    taggedPairHash',
 )
import Tahoe.CHK.SHA256d (SHA256d)
import Tahoe.CHK.Types (
    Parameters (..),
 )
import Test.Tasty (
    TestTree,
    testGroup,
 )
import Test.Tasty.HUnit (
    assertEqual,
    testCase,
 )

tests :: TestTree
tests =
    testGroup
        "Crypto"
        [ testGroup
            "sha256"
            [ testCase "sha256" $
                assertEqual
                    "known value"
                    "\x23\xfb\xe2\x1e\x2f\xae\xde\xb5\x44\x92\xcb\x7a\xc6\x0e\x04\x4a\xbb\x47\x3f\xcb\x13\x1a\x65\x8e\xd2\x5c\xd0\x17\x06\xc3\xf3\x98"
                    (sha256 "4:tag1,value")
            , testCase "sha256d" $
                assertEqual
                    "known value"
                    "\x6b\xd0\xdc\xb0\x2f\x11\x0a\xe1\xe9\x41\x1f\x12\x52\x07\x03\x66\xfe\xaa\xcb\xc9\xda\xdb\x66\xa4\xa9\xa0\xc0\xdd\x85\x49\x5d\xc4"
                    (sha256d "4:tag1,value")
            , testCase "storage index tagged hash" $
                -- Adapted from allmydata.test.test_hashutil.HashUtilTests.test_known_answers
                assertEqual
                    "known value"
                    (CryptoPassed "\xb5\x4c\x60\xc5\xb1\x26\x46\xf0\x77\x0\xc4\x4c\x8b\x75\xb9\x48")
                    (storageIndexHash <$> xKey)
            , testCase "tagged hash length" $ do
                -- The length of the result equals the given size.
                let expected = 17
                assertEqual "taggedHash result length /= expected" expected (B.length $ taggedHash expected "tag" "hello world")
                assertEqual "taggedPairHash result length /= expected" expected (B.length $ taggedPairHash expected "tag" "hello" "world")
            , testCase "well-known tagged hashes" $ do
                assertEqual
                    "taggedHash' result /= expected"
                    "yra322btzoqjp4ts2jon5dztgnilcdg6jgztgk7joi6qpjkitg2q"
                    (taggedHash' @SHA256d "tag" "hello world")
                assertEqual
                    "taggedHash' result /= expected"
                    "kfbsfssrv2bvtp3regne6j7gpdjcdjwncewriyfdtt764o5oa7ta"
                    (taggedHash' @SHA256d "different" "hello world")
                assertEqual
                    "taggedHash' result /= expected"
                    "z34pzkgo36chbjz2qykonlxthc4zdqqquapw4bcaoogzvmmcr3zq"
                    (taggedHash' @SHA256d "different" "goodbye world")
            , testCase "well-known tagged pair hashes" $ do
                assertEqual
                    "taggedPairHash' result /= expected"
                    "wmto44q3shtezwggku2fxztfkwibvznkfu6clatnvfog527sb6dq"
                    (taggedPairHash' @SHA256d "tag" "hello" "world")
                assertEqual
                    "taggedPairHash' result /= expected"
                    "lzn27njx246jhijpendqrxlk4yb23nznbcrihommbymg5e7quh4a"
                    (taggedPairHash' @SHA256d "different" "hello" "world")
                assertEqual
                    "taggedPairHash' result /= expected"
                    "qnehpoypxxdhjheqq7dayloghtu42yr55uylc776zt23ii73o3oq"
                    (taggedPairHash' @SHA256d "different" "goodbye" "world")
            , testCase "well-known ciphertext segment hashes" $ do
                assertEqual
                    "ciphertextSegmentHash' result /= expected"
                    "aovy5aa7jej6ym5ikgwyoi4pxawnoj3wtaludjz7e2nb5xijb7aa"
                    (ciphertextSegmentHash' @SHA256d "")
            , testCase "well-known block hashes" $ do
                assertEqual
                    "blockHash' result /= expected"
                    "msjr5bh4evuh7fa3zw7uovixfbvlnstr5b65mrerwfnvjxig2jvq"
                    (blockHash' @SHA256d "")
            , testCase "convergence hasher tag" $
                -- See allmydata.test.test_hashutil.HashUtilTests.test_convergence_hasher_tag
                let convergenceSecret = B.replicate convergenceSecretLength 0x42
                    params =
                        Parameters
                            { paramSegmentSize = 1024
                            , paramTotalShares = 10
                            , paramHappyShares = undefined
                            , paramRequiredShares = 3
                            }
                 in assertEqual
                        "known value"
                        ( mconcat
                            [ "allmydata_immutable_content_to_key_with_added_secret_v1+"
                            , "16:\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42,"
                            , "9:3,10,1024,"
                            ]
                        )
                        (convergenceEncryptionTag convergenceSecret params)
            ]
        ]
  where
    xKey = cipherInit keyBytes :: CryptoFailable (Key AES128)
    keyBytes = B.replicate keySize (fromIntegral $ ord 'x')
    keySize = case cipherKeySize @(Key AES128) undefined of
        KeySizeRange _ high -> high
        KeySizeEnum [] -> error "no key sizes!"
        KeySizeEnum (s : _) -> s
        KeySizeFixed s -> s
