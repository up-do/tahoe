module GeneratorsDirectory where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Tahoe.Directory (Directory (..), Entry (..))

directories :: MonadGen m => m Directory
directories = Directory <$> Gen.list (Range.exponential 0 100) entries

entries :: MonadGen m => m Entry
entries = Entry <$> entryNames <*> entryReaders <*> entryEncryptedWriters <*> entryMetadatas

entryNames :: MonadGen m => m T.Text
entryNames = Gen.text (Range.exponential 1 100) Gen.unicode

entryReaders :: MonadGen m => m B.ByteString
entryReaders = pure "URI:CHK:blub:blab"

entryEncryptedWriters :: MonadGen m => m B.ByteString
entryEncryptedWriters = pure "\1\2\3\4\5\6\7\8\9\10"

entryMetadatas :: MonadGen m => m B.ByteString
entryMetadatas = pure "{stuff: [goes, here]}"
