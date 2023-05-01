{-# LANGUAGE OverloadedStrings #-}

module CBORSpec (
    spec,
) where

import Codec.Serialise
import Data.Proxy

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set as Set
import Servant.API
import TahoeLAFS.Storage.API
import Test.Hspec (
    Spec,
    describe,
    it,
    shouldBe,
 )
import Prelude hiding (
    toInteger,
 )

spec :: Spec
spec = do
    describe "encode decode" $
        it "round trips Version1Parameters" $
            deserialise (serialise testAV)
                `shouldBe` testAV
    describe "encode decode" $
        it "round trips ApplicationVersion" $
            deserialise (serialise v1params) `shouldBe` v1params
    describe "encode decode" $
        it "round trips Version" $
            deserialise (serialise aVersion) `shouldBe` aVersion
    describe "deserialise CBOR" $
        it "works with Tahoe CBOR" $
            deserialise tahoeVersion `shouldBe` aVersion
    describe "mimeUnrender CBOR Version" $
        it "gives the correct result" $ do
            (mimeUnrender (Proxy :: Proxy CBOR) tahoeVersion :: Either String Version) `shouldBe` (Right $ Version{parameters = Version1Parameters{maximumImmutableShareSize = 232660657664, maximumMutableShareSize = 69105000000000000, availableSpace = 232660657664}, applicationVersion = "tahoe-lafs/1.18.0.post908"})
    describe "CBOR round trip" $ do
        it "works for StorageIndex" $
            deserialise (serialise ("5" :: StorageIndex)) `shouldBe` ("5" :: StorageIndex)
        it "works for ShareNumber" $
            deserialise (serialise (ShareNumber 5)) `shouldBe` ShareNumber 5
        it "works for ReadResult" $
            deserialise (serialise readRes) `shouldBe` readRes
        it "works for CBORSet" $
            deserialise (serialise cborSet) `shouldBe` cborSet

readRes = "strict bytestring" :: BS.ByteString

cborSet = CBORSet (Set.fromList $ ShareNumber <$> [1, 2, 3])

testAV :: ApplicationVersion
testAV = "tahoe-lafs/1.18.0.post908"

v1params :: Version1Parameters
v1params = Version1Parameters 232660657664 69105000000000000 232660657664

aVersion :: Version
aVersion = Version v1params testAV

tahoeVersion :: BSL.ByteString
tahoeVersion = "\162X/http://allmydata.org/tahoe/protocols/storage/v1\163X\FSmaximum-immutable-share-size\ESC\NUL\NUL\NUL6+\167\230\NULX\SUBmaximum-mutable-share-size\ESC\NUL\245\130\161\161\&4\DLE\NULOavailable-space\ESC\NUL\NUL\NUL6+\167\230\NULSapplication-versionX\EMtahoe-lafs/1.18.0.post908"
