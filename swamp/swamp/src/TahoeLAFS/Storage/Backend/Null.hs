{-# LANGUAGE OverloadedStrings #-}

module TahoeLAFS.Storage.Backend.Null (
    NullBackend (NullBackend),
) where

import qualified Data.Set as Set
import TahoeLAFS.Storage.API (
    AllocationResult (..),
    CBORSet (..),
    Version (..),
    Version1Parameters (..),
 )
import TahoeLAFS.Storage.Backend (
    Backend (..),
 )

data NullBackend = NullBackend
    deriving (Show)

instance Backend NullBackend where
    version NullBackend =
        return
            Version
                { applicationVersion = "(null)"
                , parameters =
                    Version1Parameters
                        { maximumImmutableShareSize = 0
                        , maximumMutableShareSize = 0
                        , availableSpace = 0
                        }
                }

    createImmutableStorageIndex NullBackend _ _ _ =
        return
            AllocationResult
                { alreadyHave = mempty
                , allocated = mempty
                }

    writeImmutableShare NullBackend _ _ _ _ =
        return mempty

    adviseCorruptImmutableShare NullBackend _ _ _ =
        return mempty

    getImmutableShareNumbers NullBackend _ =
        return (CBORSet $ Set.fromList [])

    readImmutableShare NullBackend _ _ _ = mempty
