module SpecDirectory where

import qualified Data.ByteString as B
import GeneratorsDirectory (directories)
import Hedgehog (forAll, property, tripping)
import System.IO (hSetEncoding, stderr, stdout, utf8)
import Tahoe.Capability (confidentiallyShow)
import qualified Tahoe.Directory as Directory
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Hedgehog (testProperty)
import Text.Megaparsec (parse)

tests :: TestTree
tests =
    testGroup
        "Directory"
        [ testCase "well-known serialized directory round-trips through parse . serialize" $ do
            original <- B.readFile "test/example.dir"
            let parsed = Directory.parse original
                serialized = Directory.serialize <$> parsed

            assertEqual "original /= serialized" (Right original) serialized
        , testProperty "Directory round-trips through serialize . parse" $
            property $ do
                directory <- forAll directories
                tripping directory Directory.serialize Directory.parse
        , testCase "well-known directory CHK read capability round-trips through parseCapability . confidentiallyShow" $ do
            -- ❯ curl -XPOST http://localhost:3456/uri?t=mkdir-immutable --data '{"foo": ["filenode", {}], "bar": ["filenode", {}], "baz": ["filenode", {}]}'
            let original = "URI:DIR2-CHK:46y5edbbxojwg5ez4lelafu5jy:fbgn7ijfxarstdxmr363et4de522n7eslnqxavymfqkoax4lc65q:3:10:63"
            let parsed = parse Directory.pReadCHK "" original
            let serialized = confidentiallyShow <$> parsed
            assertEqual "original /= serialized" (Right original) serialized
        , testCase "well-known directory CHK verify capability round-trips through parseCapability . confidentiallyShow" $ do
            -- `tahoe ls --json <read cap>` to produce verifier
            let original = "URI:DIR2-CHK-Verifier:ni4pjcqflws33ikhifsxqwvmya:fbgn7ijfxarstdxmr363et4de522n7eslnqxavymfqkoax4lc65q:3:10:63"
            let parsed = parse Directory.pVerifyCHK "" original
            let serialized = confidentiallyShow <$> parsed
            assertEqual "original /= serialized" (Right original) serialized
        , testCase "well-known directory SDMF write capability round-trips through parseCapability . confidentiallyShow" $ do
            -- `tahoe mkdir` to produce directory writer
            let original = "URI:DIR2:ez2k3glrx46svivnnrmh77uieq:c43fbv5274wmphdykpmpweq4moat5co53fvf42lg2z2xekkghm6a"
                parsed = parse Directory.pWriteSDMF "" original
                serialized = confidentiallyShow <$> parsed
            assertEqual "original /= serialized" (Right original) serialized
        , testCase "well-known directory SDMF read capability round-trips through parseCapability . confidentiallyShow" $ do
            -- `tahoe ls --json <write cap>` to produce reader
            let original = "URI:DIR2-RO:g3vdy2tlmpejr2tts7n6pyxbnu:c43fbv5274wmphdykpmpweq4moat5co53fvf42lg2z2xekkghm6a"
                parsed = parse Directory.pReadSDMF "" original
                serialized = confidentiallyShow <$> parsed
            assertEqual "original /= serialized" (Right original) serialized
        , testCase "well-known directory SDMF verify capability round-trips through parseCapability . confidentiallyShow" $ do
            -- `tahoe ls --json <write cap>` to produce verifier
            let original = "URI:DIR2-Verifier:aofaktmdrgegdvnq3vsxnccc7q:c43fbv5274wmphdykpmpweq4moat5co53fvf42lg2z2xekkghm6a"
                parsed = parse Directory.pVerifySDMF "" original
                serialized = confidentiallyShow <$> parsed
            assertEqual "original /= serialized" (Right original) serialized
        ]

main :: IO ()
main = do
    -- Hedgehog writes some non-ASCII and the whole test process will die if
    -- it can't be encoded.  Increase the chances that all of the output can
    -- be encoded by forcing the use of UTF-8 (overriding the LANG-based
    -- choice normally made).
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    defaultMain tests
