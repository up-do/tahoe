{-# LANGUAGE DataKinds #-}

module TahoeLAFS.Storage.Client (
    -- General server info
    version,
    -- Mutable or immutable
    renewLease,
    -- Immutable operations
    createImmutableStorageIndex,
    writeImmutableShare,
    readImmutableShare,
    getImmutableShareNumbers,
    adviseCorruptImmutableShare,
    -- Mutable operations
    readTestWrite,
    readMutableShares,
    getMutableShareNumbers,
    adviseCorruptMutableShare,
) where

import Data.Proxy
import Servant
import Servant.Client (
    client,
 )
import TahoeLAFS.Storage.API (
    StorageAPI,
 )

newApi :: Proxy StorageAPI
newApi = Proxy
( version
        :<|> renewLease
        :<|> createImmutableStorageIndex
        :<|> writeImmutableShare
        :<|> readImmutableShare
        :<|> getImmutableShareNumbers
        :<|> adviseCorruptImmutableShare
        :<|> readTestWrite
        :<|> readMutableShares
        :<|> getMutableShareNumbers
        :<|> adviseCorruptMutableShare
    ) = client newApi
