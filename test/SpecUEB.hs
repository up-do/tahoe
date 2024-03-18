{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module SpecUEB (tests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.List (isInfixOf)
import qualified Data.Text as T
import Generators
import Hedgehog
import Tahoe.CHK.SHA256d (zero)
import Tahoe.CHK.Types
import Tahoe.CHK.URIExtension
import Tahoe.Netstring
import Test.Tasty
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)
import Test.Tasty.Hedgehog
import Text.Megaparsec

tests :: TestTree
tests =
    testGroup
        "URIExtension"
        [ testProperty "URIExtension round-trips through put / get" prop_roundtrip
        , testCase "numeric overflow results in parse error" $ do
            let -- Get something we know is a valid serialized URI extension as
                -- a starting point for constructing something that isn't.
                validBytes = uriExtensionToBytes $ URIExtension "csr" (Parameters 1 2 3 4) (Parameters 5 6 7 8) 9 10 11 12 13 zero zero zero
                -- Replace the legitimate value of 11 for num_segments with a
                -- value that overflows Int.
                invalidBytes :: BS.ByteString
                invalidBytes =
                    replace
                        ("num_segments:" <> netstring "11")
                        ("num_segments:" <> netstring (C8.pack . show . (+ 1) . fromIntegral @Int @Integer $ maxBound))
                        validBytes

            -- Make sure we actually invalidated something
            assertBool ("invalid == " <> show invalidBytes) (validBytes /= invalidBytes)
            let parsed = parse pURIExtension "" invalidBytes
            case parsed of
                Left err -> do
                    assertBool "expected error not found" $ "above maximum allowed value" `isInfixOf` show err
                Right result -> do
                    assertFailure $ "expected parse error, got " <> show result
        ]

-- | Like Data.Text.replace but for Data.ByteString.ByteString.
replace :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
replace target replacement original = e $ T.replace (d target) (d replacement) (d original)
  where
    d = T.pack . C8.unpack
    e = C8.pack . T.unpack

prop_roundtrip :: Property
prop_roundtrip = property $ do
    ueb <- forAll genURIExtension
    tripping ueb uriExtensionToBytes (parse pURIExtension "")
