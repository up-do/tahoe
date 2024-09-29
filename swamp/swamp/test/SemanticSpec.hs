module SemanticSpec (
    spec,
) where

import Data.Data (Proxy (Proxy))
import qualified Data.Map.Strict as Map
import Network.HTTP.Types (ByteRange (ByteRangeSuffix))
import System.Directory (
    removeDirectoryRecursive,
 )
import System.IO.Temp (
    createTempDirectory,
    getCanonicalTemporaryDirectory,
 )
import Tahoe.Storage.Testing.Spec (
    makeStorageSpec,
 )
import TahoeLAFS.Storage.API (
    ReadTestWriteVectors,
    ReadVector (ReadVector),
    ShareNumber (ShareNumber),
    TestWriteVectors,
    WriteEnablerSecret (WriteEnablerSecret),
    WriteVector (WriteVector),
 )
import TahoeLAFS.Storage.Backend.Filesystem (
    FilesystemBackend (FilesystemBackend),
 )
import TahoeLAFS.Storage.Backend.Memory (
    MutableShareSize (..),
    SecretProtected (..),
    addShares,
    memoryBackend,
    shareDataSize,
    toMutableShareSize,
 )
import TahoeLAFS.Storage.Backend.Util (
    queryRangeToReadVector,
    readvToQueryRange,
 )
import Test.Hspec (
    Spec,
    context,
    describe,
    it,
    shouldBe,
 )
import Test.QuickCheck (
    property,
 )
import Test.QuickCheck.Classes (Laws (..), semigroupMonoidLaws)
import Prelude hiding (
    lookup,
    toInteger,
 )

-- | Create a Spec that checks the given Laws.
lawsCheck :: Laws -> Spec
lawsCheck Laws{lawsTypeclass, lawsProperties} =
    describe lawsTypeclass $
        mapM_ oneLawProp lawsProperties
  where
    oneLawProp (lawName, lawProp) = it lawName lawProp

spec :: Spec
spec = do
    context "utilities" $ do
        describe "toMutableShareSize" $ do
            it "finds the larger size for some cases" $ do
                toMutableShareSize (WriteVector 0 "x") <> toMutableShareSize (WriteVector 1 "x")
                    `shouldBe` MutableShareSize 0 2

                toMutableShareSize (WriteVector 0 "Hello") <> toMutableShareSize (WriteVector 1 "bye")
                    `shouldBe` MutableShareSize 0 5

                toMutableShareSize (WriteVector 0 "x") <> toMutableShareSize (WriteVector 3 "x")
                    `shouldBe` MutableShareSize 0 4

                toMutableShareSize (WriteVector 0 "Hello") <> toMutableShareSize (WriteVector 3 "world")
                    `shouldBe` MutableShareSize 0 8

        describe "addShares" $ do
            it "prepends the new write to the share storage" $ do
                let si = "storageindex"
                    enabler = WriteEnablerSecret "enabler"
                    addShares' = addShares si enabler
                    shareNum = ShareNumber 0
                    Just a = addShares' mempty (Map.fromList [(shareNum, [WriteVector 1 "first"])])
                    Just b = addShares' a (Map.fromList [(shareNum, [WriteVector 2 "second"])])

                b `shouldBe` Map.fromList [(si, SecretProtected enabler (Map.fromList [(shareNum, [WriteVector 2 "second", WriteVector 1 "first"])]))]

            it "puts later elements in a single WriteVector list earlier in the MutableShareStorage list" $ do
                let si = "storageindex"
                    enabler = WriteEnablerSecret "enabler"
                    addShares' = addShares si enabler
                    shareNum = ShareNumber 0
                    Just a = addShares' mempty (Map.fromList [(shareNum, [WriteVector 1 "first", WriteVector 2 "second"])])

                a `shouldBe` Map.fromList [(si, SecretProtected enabler (Map.fromList [(shareNum, [WriteVector 2 "second", WriteVector 1 "first"])]))]

        describe "shareDataSize" $ do
            it "converts list of WriteVector to a size" $ do
                shareDataSize [WriteVector 2 "foo", WriteVector 10 "quux"]
                    `shouldBe` 14
                shareDataSize [WriteVector 0 "foobar", WriteVector 2 "q"]
                    `shouldBe` 6
                shareDataSize []
                    `shouldBe` 0
                shareDataSize [WriteVector 2 "foo", WriteVector 3 "quux"]
                    `shouldBe` 7

        describe "TestWriteVectors"
            . lawsCheck
            . semigroupMonoidLaws
            $ (Proxy :: Proxy TestWriteVectors)

        describe "ReadTestWriteVectors"
            . lawsCheck
            . semigroupMonoidLaws
            $ (Proxy :: Proxy ReadTestWriteVectors)

        describe "ReadVector" $ do
            it "it round-trips through queryRangeToReadVector / readvToQueryRange" $
                property $ \rvs ->
                    (queryRangeToReadVector 1234 . readvToQueryRange) rvs `shouldBe` rvs

            it "imposes a lower bound of zero on offset" $ do
                queryRangeToReadVector 1 (Just [ByteRangeSuffix 2]) `shouldBe` [ReadVector 0 1]

    context "memory" $ makeStorageSpec memoryBackend cleanupMemory
    context "filesystem" $ makeStorageSpec filesystemBackend cleanupFilesystem

filesystemBackend :: IO FilesystemBackend
filesystemBackend = do
    FilesystemBackend <$> createTemporaryDirectory

createTemporaryDirectory :: IO FilePath
createTemporaryDirectory = do
    parent <- getCanonicalTemporaryDirectory
    createTempDirectory parent "gbs-semanticspec"

cleanupFilesystem :: FilesystemBackend -> IO ()
cleanupFilesystem (FilesystemBackend path) = removeDirectoryRecursive path

cleanupMemory :: (Applicative f) => p -> f ()
cleanupMemory _ = pure ()
