{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tahoe.CHK.Server where

import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
 )
import qualified Data.ByteString as B
import Data.ByteString.Base32 (decodeBase32Unpadded, encodeBase32Unpadded)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Tahoe.CHK.Crypto (sha1, sha256)
import Tahoe.CHK.Types (Offset, ShareNum, StorageIndex)

-- Where can a server be found
type URL = T.Text

-- The unique identifier for a particular storage server, conventionally the
-- lowercase base32 encoding of some public key controlled by the server.
type StorageServerID = T.Text

-- | An announcement from a storage server about its storage service.
data StorageServerAnnouncement = StorageServerAnnouncement
    { storageServerAnnouncementFURL :: Maybe URL
    , storageServerAnnouncementNick :: Maybe T.Text
    , storageServerAnnouncementPermutationSeed :: Maybe B.ByteString
    }
    deriving (Eq, Ord, Show)

-- A server that can have some data uploaded to it.
data StorageServer = StorageServer
    { storageServerID :: StorageServerID
    , -- TODO Strict byte strings here are unfortunate.  They will force whole
      -- chunks of data into memory at once.
      storageServerWrite :: StorageIndex -> ShareNum -> Offset -> B.ByteString -> IO ()
    , storageServerRead :: StorageIndex -> ShareNum -> IO B.ByteString
    , storageServerGetBuckets :: StorageIndex -> IO (Set.Set ShareNum)
    }

instance Eq StorageServer where
    a == b = storageServerID a == storageServerID b

instance Ord StorageServer where
    a <= b = storageServerID a <= storageServerID b

instance Show StorageServer where
    show ss = show $ storageServerID ss

type ShareMap = Map.Map ShareNum (Set.Set StorageServer)

instance FromJSON StorageServerAnnouncement where
    parseJSON = withObject "StorageServerAnnouncement" $ \ann -> do
        v <- ann .: "ann"
        storageServerAnnouncementFURL <- v .:? "anonymous-storage-FURL"
        storageServerAnnouncementNick <- v .:? "nickname"
        permutationSeed <- v .:? "permutation-seed-base32"
        let storageServerAnnouncementPermutationSeed =
                case permutationSeed of
                    Nothing -> Nothing
                    Just txt -> case decodeBase32Unpadded . encodeUtf8 $ txt of
                        Left _ -> Nothing
                        Right ps -> Just ps

        pure StorageServerAnnouncement{..}

instance ToJSON StorageServerAnnouncement where
    toJSON StorageServerAnnouncement{..} =
        object
            [ "ann"
                .= object
                    [ "anonymous-storage-FURL" .= storageServerAnnouncementFURL
                    , "nickname" .= storageServerAnnouncementNick
                    , "permutation-seed-base32"
                        .= (encodeBase32Unpadded <$> storageServerAnnouncementPermutationSeed)
                    ]
            ]

{- | Find the preferred order of servers for an object with the given index.

 This is like allmydata.storage_client.StorageFarmBroker.get_servers_for_psi
-}
preferredServers :: StorageIndex -> Map.Map T.Text StorageServerAnnouncement -> [(StorageServerID, StorageServerAnnouncement)]
preferredServers storageIndex = sortOn permutedServerHash . Map.toList
  where
    permutedServerHash =
        -- allmydata.util.hashutil.permute_server_hash
        sha1 . (storageIndex <>) . uncurry storageServerPermutationSeed

{- | Compute a sort key for a storage server given its identifier and storage
 service announcement.

 This is like pieces of allmydata.storage_client._parse_announcement
-}
storageServerPermutationSeed :: StorageServerID -> StorageServerAnnouncement -> B.ByteString
storageServerPermutationSeed serverId ann =
    case storageServerAnnouncementPermutationSeed ann of
        Just bs -> bs
        Nothing ->
            case decodeBase32Unpadded . encodeUtf8 . T.drop 3 $ serverId of
                Right bs -> bs
                Left _ -> sha256 . encodeUtf8 $ serverId
