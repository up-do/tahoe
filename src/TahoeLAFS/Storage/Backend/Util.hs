module TahoeLAFS.Storage.Backend.Util where

import Network.HTTP.Types (ByteRange (..))
import Tahoe.Storage.Backend (QueryRange, ReadVector (..), Size)

readvToByteRange :: ReadVector -> ByteRange
readvToByteRange ReadVector{..} = ByteRangeFromTo offset (offset + readSize - 1)

readvToQueryRange :: [ReadVector] -> QueryRange
readvToQueryRange = Just . fmap readvToByteRange

queryRangeToReadVector :: Size -> QueryRange -> [ReadVector]
queryRangeToReadVector shareSize Nothing = [ReadVector 0 shareSize]
queryRangeToReadVector shareSize (Just ranges) = toReadVector <$> ranges
  where
    toReadVector (ByteRangeFrom start) = ReadVector offset size
      where
        offset = max 0 start
        size = shareSize - offset
    toReadVector (ByteRangeFromTo start end) = ReadVector offset size
      where
        offset = min shareSize (max 0 start)
        size = min (shareSize - offset) (end - start + 1)
    toReadVector (ByteRangeSuffix len) = ReadVector offset size
      where
        offset = max 0 $ shareSize - len
        size = min (shareSize - offset) len
