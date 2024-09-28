{-# LANGUAGE OverloadedStrings #-}

module SpecUpload (
    tests,
) where

import Data.ByteArray (convert)
import Data.ByteString.Base32 (
    encodeBase32Unpadded,
 )
import Data.Maybe (mapMaybe)

import Test.Tasty (
    TestTree,
    testGroup,
 )

import Test.Tasty.HUnit (
    assertEqual,
    testCase,
 )

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Tahoe.CHK.Types (
    Parameters (..),
    Required,
    Size,
 )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Generators (storageServerAnnouncements, storageServerIdentifiers)
import Hedgehog (Property, diff, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Tahoe.CHK.Server (
    StorageServerAnnouncement (..),
    preferredServers,
 )
import Tahoe.CHK.Upload (
    adjustSegmentSize,
    getConvergentKey,
 )
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
    testGroup
        "Upload"
        [ testConvergence
        , testAdjustSegmentSize
        ]

testAdjustSegmentSize :: TestTree
testAdjustSegmentSize =
    testGroup
        "adjustSegmentSize"
        [ testCase "same size" $
            -- If the data size is the same as the segment size, leave the segment
            -- size alone.
            assertEqual "" (p 3 3) (adjustSegmentSize (p 3 3) 3)
        , testCase "bigger" $
            -- If the data size is larger than the segment size the segment size can
            -- be left alone.
            assertEqual "" (p 3 3) (adjustSegmentSize (p 3 3) 50)
        , testCase "smaller, already multiple" $
            -- If the data size is smaller than the segment size and also a multiple
            -- of the required value, make the segment size the data size.
            assertEqual "" (p 3 3) (adjustSegmentSize (p 4 3) 3)
        , testCase "smaller, not multiple" $
            -- If the data size is smaller than the segment size but not a multiple of
            -- the required value, make the segment size the next multiple of required
            -- larger than the data size.
            assertEqual "" (p 3 3) (adjustSegmentSize (p 3 3) 2)
        , testCase "regression" $
            assertEqual "" (p 131073 3) (adjustSegmentSize (p 131072 3) 104857600)
        , testProperty "preferredServers returns all of the servers passed to it" prop_preferredServers_all
        , testCase "preferredServers orders servers just like Tahoe-LAFS" $
            -- Two cases from allmydata.test.test_client.Basic.test_permute
            let makeServer :: Int -> (Text, StorageServerAnnouncement)
                makeServer n =
                    ( encodeBase32Unpadded . encodeUtf8 . pack . show $ n
                    , StorageServerAnnouncement
                        { storageServerAnnouncementFURL = Just "pb://somewhere/something"
                        , storageServerAnnouncementNick = Just . pack . show $ n
                        , storageServerAnnouncementPermutationSeed = Just . encodeUtf8 . pack . show $ n
                        }
                    )
                servers = Map.fromList $ makeServer <$> [0 .. 4]
                ident = storageServerAnnouncementNick . snd
                one = mapMaybe ident $ preferredServers "one" servers
                two = mapMaybe ident $ preferredServers "two" servers
             in do
                    assertEqual "case one" ["3", "1", "0", "4", "2"] one
                    assertEqual "case two" ["0", "4", "2", "1", "3"] two
        ]
  where
    p :: Size -> Required -> Parameters
    p segmentSize = Parameters segmentSize 5 4

prop_preferredServers_all :: Property
prop_preferredServers_all = property $ do
    -- XXX 32 bytes?
    storageIndex <- forAll $ Gen.bytes (Range.singleton 32)
    servers <- forAll $ Gen.map (Range.linear 1 30) ((,) <$> storageServerIdentifiers <*> storageServerAnnouncements)
    let preferred = preferredServers storageIndex servers
    diff (Set.fromList . Map.toList $ servers) (==) (Set.fromList preferred)

testConvergence :: TestTree
testConvergence =
    testGroup
        "Convergence"
        [ testCase "getConvergentKey short data" $
            uncurry verifyConvergentKey short
        , testCase "getConvergentKey medium data" $
            uncurry verifyConvergentKey medium
        , testCase "getConvergentKey long data" $
            uncurry verifyConvergentKey long
        ]
  where
    --
    -- These known results are correct because this Python program emits them:
    --
    -- from allmydata.util.hashutil import convergence_hasher
    -- from allmydata.util.base32 import b2a
    -- hasher = convergence_hasher(3, 10, 1024, "\x42" * 32)
    -- hasher.update(dataContent)
    -- print(b2a(hasher.digest()).upper())
    --
    -- using Tahoe-LAFS git revision
    -- 4e4114486710a2e88d494dcb6c0adf9e356173e7 (though the convergence
    -- function should be extremely stable so it is likely that most
    -- versions of Tahoe-LAFS will produce the same result).
    --
    short =
        ( "Hello, world."
        , "3A6SKSC36YBRRZUJBN4IX4WRGU"
        )
    medium =
        ( B.concat $ replicate 256 "01234567"
        , "VEUFRBTL3EBX7WP3SNZL2HCQOU"
        )
    long =
        ( B.concat $ replicate (1024 * 1024) "01234567"
        , "KZXNCMP427WO37EEMH7TJYJQ3M"
        )

    verifyConvergentKey dataContent expectedKeyBytes =
        assertEqual
            "The key matches the known correct result"
            expectedKeyBytes
            (encodeBase32Unpadded . convert $ key)
      where
        key = getConvergentKey secret params (BL.fromStrict dataContent)

    secret = B.replicate 32 0x42
    params =
        Parameters
            { paramSegmentSize = 1024
            , paramTotalShares = 10
            , paramHappyShares = 7
            , paramRequiredShares = 3
            }
