module SpecServer (tests) where

import Control.Monad (void, zipWithM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (decode, encode)
import qualified Data.ByteString as BS
import Generators (shareNumbers, storageIndexes, storageServerAnnouncements, storageServerIdentifiers)
import Hedgehog (Property, diff, forAll, property, tripping)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Tahoe.CHK.Server (StorageServer (storageServerRead, storageServerWrite))
import Tahoe.Server (directoryStorageServer', memoryStorageServer)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
    testGroup
        "Server"
        [ testProperty
            "immutable data round-trips through memoryStorageServer write / read"
            (prop_immutable_tripping memoryStorageServer)
        , testProperty
            "immutable data round-trips through directoryStorageServer write / read"
            (prop_immutable_tripping (directoryStorageServer' "SpecServer-storage"))
        , testProperty
            "Maps of StorageServerAnnouncement round-trip through JSON"
            prop_servers_json_tripping
        ]

prop_servers_json_tripping :: Property
prop_servers_json_tripping = property $ do
    servers <- forAll $ Gen.map (Range.linear 1 10) ((,) <$> storageServerIdentifiers <*> storageServerAnnouncements)
    tripping servers encode decode

prop_immutable_tripping :: IO StorageServer -> Property
prop_immutable_tripping newServer = property $ do
    server <- liftIO newServer
    storageIndex <- forAll storageIndexes
    shareNum <- forAll shareNumbers
    shareChunks <- forAll $ Gen.list (Range.linear 1 100) (Gen.bytes (Range.linear 1 100))

    let write' = storageServerWrite server storageIndex shareNum
        read' = storageServerRead server storageIndex shareNum
        offsets = scanl (flip $ (+) . BS.length) 0 shareChunks

    void . liftIO $ zipWithM_ (write' . fromIntegral) offsets shareChunks

    result <- liftIO read'
    diff (BS.concat shareChunks) (==) result
