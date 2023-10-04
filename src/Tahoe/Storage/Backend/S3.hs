module Tahoe.Storage.Backend.S3 where

import Amazonka (runResourceT)
import qualified Amazonka as AWS
import qualified Amazonka.S3 as S3
import TahoeLAFS.Storage.API (AllocateBuckets (..), AllocationResult (..))
import TahoeLAFS.Storage.Backend (Backend (..))

data S3Backend = S3Backend AWS.Env S3.BucketName

instance Backend S3Backend where
    createImmutableStorageIndex (S3Backend env name) storageIndex (AllocateBuckets{shareNumbers, allocatedSize}) =
        runResourceT $ do
            objects <- AWS.send env (S3.newListObjectsV2 name)
            pure $ AllocationResult{alreadyHave = [], allocated = []}
