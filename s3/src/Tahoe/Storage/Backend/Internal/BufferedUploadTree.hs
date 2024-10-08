{-# LANGUAGE MultiParamTypeClasses #-}

module Tahoe.Storage.Backend.Internal.BufferedUploadTree where

import qualified Data.ByteString.Lazy as B
import Data.FingerTree (ViewL ((:<)), ViewR ((:>)), (<|), (><), (|>))
import qualified Data.FingerTree as FT
import Data.List (unfoldr)
import Tahoe.Storage.Backend (Size)

data Interval = Interval
    { intervalLow :: Size
    , intervalHigh :: Size
    }
    deriving (Eq)

instance Show Interval where
    show (Interval l h) = "[" <> show l <> ".." <> show h <> "]"

instance Semigroup Interval where
    Interval lowA highA <> Interval lowB highB = Interval l h
      where
        l = min lowA lowB
        h = max highA highB

-- XXX Not a very good Monoid instance.
instance Monoid Interval where
    mempty = Interval 0 0

low, high :: Interval -> Size
(low, high) = (intervalLow, intervalHigh)

intervalSize :: Interval -> Size
intervalSize (Interval l h) = h - l + 1

{- | Split an interval into two where the first has the first `after` bytes
 and the second has the rest.
-}
splitIntervalAfter :: Integer -> Interval -> (Interval, Interval)
splitIntervalAfter after (Interval intervalLow intervalHigh) =
    ( Interval intervalLow (intervalLow + after - 1)
    , Interval (intervalLow + after) intervalHigh
    )

-- | Does the first interval contain the entire second interval?
containsInterval :: Interval -> Interval -> Bool
containsInterval a b =
    intervalLow b >= intervalLow a && intervalHigh b <= intervalHigh a

data Part backend uploadResponse
    = PartData
        { getInterval :: Interval
        , getShareData :: B.ByteString
        , totalShareSize :: Size
        }
    | PartUploading {getPartNumber :: PartNumber, getInterval :: Interval}
    | PartUploaded {getPartNumber :: PartNumber, getPartResponse :: uploadResponse, totalShareSize :: Size}
    deriving (Eq, Show)

newtype PartNumber = PartNumber Integer
    deriving newtype (Ord, Eq, Enum, Num, Real, Integral)
    deriving (Show)

data UploadInfo = UploadInfo
    { uploadInfoPartNumber :: PartNumber
    , uploadInfoBytes :: B.ByteString
    }
    deriving (Eq, Show)

data UploadTreeMeasure backend = UploadTreeMeasure
    { -- | A "bounding box" interval that covers the intervals of all of the
      -- nodes measured.  No node will have an interval outside this value but
      -- not all positions inside this interval necessarily have bytes.
      coveringInterval :: Interval
    , -- | The largest number of contiguous bytes of any node measured.
      contiguousBytes :: Size
    , -- | The largest number of fixed-interval pieces covered by a node
      -- measured.
      uploadableParts :: Size
    , -- | Is every node measured uploaded already.
      fullyUploaded :: Bool
    }
    deriving (Eq, Show)

instance Semigroup (UploadTreeMeasure backend) where
    (UploadTreeMeasure aInt aContig aParts aUp) <> (UploadTreeMeasure bInt bContig bParts bUp) =
        UploadTreeMeasure (aInt <> bInt) (max aContig bContig) (max aParts bParts) (aUp && bUp)

instance Monoid (UploadTreeMeasure backend) where
    mempty = UploadTreeMeasure mempty 0 0 True

newtype PartSize a = PartSize Size
    deriving newtype (Eq, Ord, Enum, Num, Real, Integral)

class IsBackend backend where
    minPartSize :: PartSize backend
    computePartSize :: Size -> PartSize backend

instance IsBackend backend => FT.Measured (UploadTreeMeasure backend) (Part backend a) where
    measure PartData{totalShareSize, getInterval} =
        UploadTreeMeasure
            { coveringInterval = getInterval
            , contiguousBytes = intervalSize getInterval
            , uploadableParts = uploadableParts
            , fullyUploaded = False
            }
      where
        PartSize size = computePartSize totalShareSize :: PartSize backend
        uploadableParts =
            case intervalLow getInterval `mod` size of
                0 -> intervalSize getInterval `div` size
                -- Since the interval is not aligned, some prefix of it will
                -- not be uploadable.  Account for those bytes (by ignoring
                -- them) when computing the size of the interval to divide by
                -- part size.
                n -> (intervalHigh getInterval - (intervalLow getInterval + (size - n)) + 1) `div` size
    measure PartUploading{getInterval} = UploadTreeMeasure getInterval 0 0 False
    measure PartUploaded{totalShareSize, getPartNumber} = UploadTreeMeasure (partNumberToInterval totalShareSize partSize getPartNumber) 0 0 True
      where
        partSize = computePartSize @backend totalShareSize

type UploadTree backend a = FT.FingerTree (UploadTreeMeasure backend) (Part backend a)

{- | Compute the interval covering the last part of a share with the given
 size and part size.
-}
finalInterval :: Size -> PartSize backend -> Interval
finalInterval totalSize (PartSize partSize) = Interval start (end - 1)
  where
    end = totalSize

    start =
        end - case totalSize `mod` partSize of
            -- The total size is an exact multiple of part size.  The final interval
            -- covers a "full" part (not a "short" part).
            0 -> partSize
            -- It is not an exact multiple so the final interval is a "short" part.
            n -> n

{- | Find all chunks of uploadable data and return them, along with a tree
 updated with respect to each found chunk, in the manner that
 findUploadableChunk would update it.
-}
findUploadableChunks :: forall backend a. (IsBackend backend, Show a) => UploadTree backend a -> ([UploadInfo], UploadTree backend a)
findUploadableChunks t = if null allTrees then ([], t) else (infos, last allTrees)
  where
    (infos, allTrees) = unzip $ unfoldr f t
    f tree = case findUploadableChunk tree 1 of
        (Nothing, _) -> Nothing
        (Just info, updatedTree) -> Just ((info, updatedTree), updatedTree)

--       1     2     3     4     5
-- 1  |.....|.....|....X|XXXX.|.....|
-- 2  |.....|YY...|....X|XXXX.|.....|
-- 2.5|.....|YYYYY|....X|XXXX.|.....|
-- 3  |.....|yyyyy|....X|XXXX.|ZZZZZ|
-- 4  |WWWWW|yyyyy|....X|XXXX.|zzzzz|
-- 5  |wwwww|yyyyy|AAAAX|XXXX.|zzzzz|
-- 6  |wwwww|yyyyy|aaaax|XXXXB|zzzzz|
-- 7  |wwwww|yyyyy|aaaax|xxxxb|zzzzz|

--       1     2     3     4     5
-- 1  |.....|.....|....A|XXXX.|.....|
-- 2  |.....|.....|....A|XXXX.|.....|
-- 3  |Y....|.....|....A|XXXX.|.....|
-- 3  |Y....|Z....|....A|XXXX.|.....|
-- 3  |Y....|Z....|....A|XXXX.|.....|
-- 3  |YYYYY|Z....|....A|XXXX.|W....|
-- 3  |yyyyy|ZZZZZ|AAAAA|XXXX.|W....|

{- | Find one chunk of uploadable data and return it, along with an updated
 tree which has marked that chunk as being uploaded.
-}
findUploadableChunk :: forall backend a. (IsBackend backend, Show a) => UploadTree backend a -> Integer -> (Maybe UploadInfo, UploadTree backend a)
findUploadableChunk uploadTree minParts = (upload, tree')
  where
    position :: UploadTreeMeasure backend -> Bool
    position m = uploadableParts m >= minParts

    (left, right) = FT.split position uploadTree

    (upload, tree') = case FT.viewl right of
        --
        -- If we traverse the whole tree (even if only because the tree was
        -- empty) without the predicate flipping, nothing is uploadable
        FT.EmptyL ->
            case FT.viewr left of
                -- There is no rightmost element so there can't be anything to
                -- upload.
                FT.EmptyR -> (Nothing, uploadTree)
                -- The rightmost element is PartData which might represent a
                -- short final part.  If it crosses the last part boundary
                -- /and/ extends to the end of the share, it does.
                lefties :> PartData{getInterval, getShareData, totalShareSize} ->
                    if shortFinalPart && getInterval `containsInterval` finalPartInterval
                        then (Just uploadInfo, lefties >< newTree)
                        else (Nothing, uploadTree)
                  where
                    shortFinalPart = finalPartSize /= partSize
                    finalPartSize = PartSize $ intervalSize finalPartInterval
                    finalPartInterval = finalInterval totalShareSize partSize

                    (uploadInfo, newTree) = splitPart getInterval getShareData finalPartSize

                    splitPart interval bytes (PartSize size) = (ui, tree)
                      where
                        partNum = assignPartNumber @backend ri partSize
                        ui = UploadInfo partNum rb
                        up = PartUploading partNum ri

                        tree = FT.fromList $
                            case leftSize of
                                0 -> [up]
                                _ -> [PartData li lb totalShareSize, up]

                        leftSize = intervalSize interval - size
                        (lb, rb) = B.splitAt (fromIntegral leftSize) bytes
                        (li, ri) = splitIntervalAfter leftSize interval

                    partSize = computePartSize @backend totalShareSize
                -- The rightmost element might already have been uploaded.
                _ :> _ -> (Nothing, uploadTree)
        --
        -- Predicate flipped: the matching element is uploadable.  Extract it.
        PartData{getInterval, getShareData, totalShareSize} :< righties ->
            (Just uploadInfo, left >< newTree >< righties)
          where
            (uploadInfo, newTree) = computeNewTree getInterval getShareData totalShareSize
        --
        -- It shouldn't be possible to get anything except a PartData in the
        -- first position due to the way our measurement is defined.
        otherPart :< _ ->
            error $ "EmptyR case of findUploadableChunk expected PartData, got: " <> show otherPart

assignPartNumber :: Interval -> PartSize backend -> PartNumber
assignPartNumber Interval{intervalLow} (PartSize partSize) = PartNumber $ 1 + intervalLow `div` partSize

computeNewTree ::
    forall backend response.
    IsBackend backend =>
    Interval ->
    B.ByteString ->
    Size ->
    (UploadInfo, FT.FingerTree (UploadTreeMeasure backend) (Part backend response))
computeNewTree getInterval getShareData totalShareSize = (uploadInfo, newTree)
  where
    -- A description of the uploadable chunk that was found.
    uploadInfo = UploadInfo partNum chunkBytes

    -- A new tree made out of pieces of data that were contiguous with the
    -- uploadable chunk but which were not themselves uploadable, and a
    -- replacement for the uploadable chunk describing the fact that it is
    -- being uploaded.
    newTree = pr >< PartUploading partNum uploadableInterval <| suf

    -- The part number assigned to the part the uploadable chunk will be used
    -- to create.
    partNum = assignPartNumber uploadableInterval (PartSize partSize)

    -- The prefix of the tree formed after the removal of the uploadable chunk.
    pr
        | prefixLength == 0 = mempty
        | otherwise = PartData prefixInterval prefixBytes totalShareSize <| mempty

    -- The suffix of the tree formed after the removal of the uploadable chunk.
    suf
        | suffixLength == 0 = mempty
        | otherwise = mempty |> PartData suffixInterval suffixBytes totalShareSize

    -- How many bytes are attached to the left of the chunk which cannot be
    -- used because they do not begin on a part boundary?
    prefixLength = case intervalLow getInterval `mod` partSize of
        0 -> 0
        n -> partSize - n

    -- How many bytes are attached to the right of the chunk which cannot be
    -- used because they do not end on a part boundary?
    suffixLength = (intervalHigh getInterval + 1) `mod` partSize

    -- How many bytes from the chunk are usable for upload?
    chunkLength = intervalSize getInterval - prefixLength - suffixLength

    (prefixBytes, chunkAndSuffix) = B.splitAt (fromIntegral prefixLength) getShareData
    (chunkBytes, suffixBytes) = B.splitAt (fromIntegral chunkLength) chunkAndSuffix

    -- How many bytes are in each "part" for this tree?
    partSize = fromIntegral $ computePartSize @backend totalShareSize

    -- Three intervals (prefixInterval, uploadableInterval, suffixInterval) describing:
    -- 1. unusable bytes attached to the left of the chunk.
    -- 2. usable bytes from the chunk.
    -- 3. unusable bytes attached to the right of the chunk.
    (prefixInterval, more) = splitIntervalAfter prefixLength getInterval
    (uploadableInterval, suffixInterval) = splitIntervalAfter chunkLength more

{- | Compute the interval covered by the given part number, for a share of a
 certain size.
-}
partNumberToInterval :: Size -> PartSize backend -> PartNumber -> Interval
partNumberToInterval totalSize (PartSize partSize) (PartNumber n) = Interval l h
  where
    l = (n - 1) * partSize
    h = min totalSize (n * partSize) - 1

replace :: IsBackend backend => Interval -> Part backend a -> UploadTree backend a -> Maybe (UploadTree backend a)
replace interval newPart uploadTree =
    case (left, right') of
        (lefties, _target :< righties) ->
            -- The new part might completely or only partially overlap with the
            -- part being replaced.  If it partially overlaps, leave behind
            -- whatever parts of target aren't covered by the new part.
            -- Otherwise, drop the new part in place of the target.
            --
            -- XXX Deal with the partial overlaps.  Maybe by making sure all
            -- nodes in the tree are actually part boundary aligned?
            Just $ (lefties |> newPart) >< righties
        _ ->
            -- Any other result means we didn't find a node relate to the
            -- PartNumber in the way we wanted.  Abandon the replacement
            -- attempt.  Ideally this case is impossible ...
            Nothing
  where
    (left, right) = splitInterval interval uploadTree
    right' = FT.viewl right

splitInterval ::
    IsBackend backend =>
    Interval ->
    FT.FingerTree (UploadTreeMeasure backend) (Part backend a) ->
    ( FT.FingerTree (UploadTreeMeasure backend) (Part backend a)
    , FT.FingerTree (UploadTreeMeasure backend) (Part backend a)
    )
splitInterval interval = FT.split predicate
  where
    -- Our predicate should flip from "false" -> "true" _on_ the element we
    -- want to insert before
    predicate measure = intervalLow interval <= intervalHigh (coveringInterval measure)

insert ::
    IsBackend backend =>
    Part backend a ->
    UploadTree backend a ->
    UploadTree backend a
insert p uploadTree = tree'
  where
    (left, right) = splitInterval (getInterval p) uploadTree

    tree' = case (FT.viewr left, FT.viewl right) of
        -- If there's nothing around to merge with then the inserted value is
        -- the whole resulting tree.
        (FT.EmptyR, FT.EmptyL) -> FT.singleton p
        -- If we can only grab a node from the left, only try to merge with it.
        (lefties :> before, FT.EmptyL) -> lefties >< FT.fromList (merge2' before p)
        -- Likewise if we can only grab a node from the right.
        (FT.EmptyR, after :< righties) -> FT.fromList (merge2' p after) >< righties
        -- And if we can get both, try merging both ways.
        (lefties :> before, after :< righties) -> lefties >< FT.fromList (merge3 before p after) >< righties

merge3 :: Part backend a -> Part backend a -> Part backend a -> [Part backend a]
merge3 a b c =
    case merge2 a b of
        Nothing -> case merge2 b c of
            Nothing -> [a, b, c]
            Just bc -> [a, bc]
        Just ab -> case merge2 ab c of
            Nothing -> [ab, c]
            Just abc -> [abc]

merge2 :: Part backend a -> Part backend a -> Maybe (Part backend a)
merge2 (PartData aInt aData aSize) (PartData bInt bData bSize)
    | aSize /= bSize = error $ "Cannot merge PartData with different sizes: " <> show aSize <> " " <> show bSize
    | succ (high aInt) == low bInt = Just (PartData (aInt <> bInt) (aData <> bData) aSize)
    | otherwise = Nothing
merge2 _ _ = Nothing

merge2' :: Part backend a -> Part backend a -> [Part backend a]
merge2' a b = maybe [a, b] (: []) $ merge2 a b
