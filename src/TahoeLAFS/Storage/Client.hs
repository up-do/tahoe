{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TahoeLAFS.Storage.Client (
    version,
    createImmutableStorageIndex,
    writeImmutableShare,
    adviseCorruptImmutableShare,
    getImmutableShareNumbers,
    readImmutableShares,
    createMutableStorageIndex,
    readvAndTestvAndWritev,
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

type NewApi = "storage" :> StorageAPI

newApi :: Proxy NewApi
newApi = Proxy
( version
        :<|> createImmutableStorageIndex
        :<|> writeImmutableShare
        :<|> adviseCorruptImmutableShare
        :<|> getImmutableShareNumbers
        :<|> readImmutableShares
        :<|> createMutableStorageIndex
        :<|> readvAndTestvAndWritev
        :<|> readMutableShares
        :<|> getMutableShareNumbers
        :<|> adviseCorruptMutableShare
    ) = client newApi
