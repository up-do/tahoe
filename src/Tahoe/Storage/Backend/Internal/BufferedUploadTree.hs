{-# LANGUAGE MultiParamTypeClasses #-}

module Tahoe.Storage.Backend.Internal.BufferedUploadTree (
    IsBackend (..),
    Interval (..),
    intervalSize,
    Part (..),
    PartSize (..),
    PartNumber (..),
    UploadInfo (..),
    UploadTreeMeasure (..),
    UploadTree (..),
    highestPartNumber,
    offsetToPartNumber,
    partNumberToInterval,
    findUploadableChunk,
    replace,
    insert,
) where

import qualified Data.ByteString as B
import Data.FingerTree (ViewL ((:<)), ViewR ((:>)), (<|), (><), (|>))
import qualified Data.FingerTree as FT
import Tahoe.Storage.Backend (Size)

data Interval = Interval
    { intervalLow :: Size
    , intervalHigh :: Size
    }
    deriving (Eq)

instance Show Interval where
    show (Interval l h) = "[" <> show l <> ".." <> show h <> "]"

instance Semigroup Interval where
    Interval lowA highA <> Interval lowB highB = Interval (min lowA lowB) (max highA highB)

instance Monoid Interval where
    mempty = Interval 0 0

low, high :: Interval -> Size
(low, high) = (intervalLow, intervalHigh)

intervalSize :: Interval -> Size
intervalSize (Interval l h) = h - l + 1

data Part backend uploadResponse
    = PartData
        { getInterval :: Interval
        , getShareData :: B.ByteString
        , totalShareSize :: Size
        }
    | PartUploading {getPartNumber :: PartNumber, getInterval :: Interval}
    | PartUploaded {getPartNumber :: PartNumber, getPartResponse :: uploadResponse}
    deriving (Eq, Show)

newtype PartNumber = PartNumber Integer
    deriving newtype (Ord, Eq)
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

-- measure "abc" == 3
-- measure "wxyz" == 4
-- measure ("abc" <> "wxyz") == 7
-- measure "abc" <> measure "wxyz" == 7

-- measure (x <> y) == (measure x) <> (measure y)
--

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
    measure PartUploaded{getPartNumber} = UploadTreeMeasure (partNumberToInterval getPartNumber) 0 0 True

newtype UploadTree backend a = UploadTree
    { uploadTree :: FT.FingerTree (UploadTreeMeasure backend) (Part backend a)
    }
    deriving newtype (Semigroup, Monoid)
    deriving (Eq, Show)

findUploadableChunk :: forall backend a. (IsBackend backend, Show a) => (Interval -> PartNumber) -> UploadTree backend a -> Integer -> (Maybe UploadInfo, UploadTree backend a)
findUploadableChunk assignNumber t@UploadTree{uploadTree} minParts =
    (upload, t{uploadTree = tree'})
  where
    position :: UploadTreeMeasure backend -> Bool
    position m = uploadableParts m >= minParts

    (left, right) = FT.split position uploadTree

    (upload, tree') = case (FT.viewr left, FT.viewl right) of
        --
        -- If we traverse the whole tree (even if only because the tree was
        -- empty) without the predicate flipping, nothing is uploadable
        (_, FT.EmptyL) -> (Nothing, uploadTree)
        --
        -- Predicate flipped immediately: whole tree is uploadable
        (FT.EmptyR, (PartData{getInterval, getShareData, totalShareSize}) :< righties) ->
            ( Just (UploadInfo partNum uploadable)
            , pr >< PartUploading partNum (Interval intstart intend) <| suf >< righties
            )
          where
            partNum = assignNumber getInterval
            partSize = fromIntegral $ computePartSize @backend totalShareSize
            prefix = intervalLow getInterval `mod` partSize
            suffix = (intervalHigh getInterval + 1) `mod` partSize
            droppable
                | suffix == 0 = fromIntegral $ intervalSize getInterval
                | otherwise = fromIntegral (intervalSize getInterval - suffix)
            takable
                | prefix == 0 = 0
                | otherwise = fromIntegral (intervalSize lowint)
            lowint = Interval (intervalLow getInterval) (intervalLow getInterval + (partSize - prefix))
            highint = Interval (intervalHigh getInterval - suffix) (intervalHigh getInterval)
            pr
                | prefix == 0 = mempty
                | otherwise = PartData lowint (B.take takable getShareData) totalShareSize <| mempty
            suf
                | suffix == 0 = mempty
                | otherwise = mempty |> PartData highint (B.drop droppable getShareData) totalShareSize
            uploadable = B.drop takable . B.take droppable $ getShareData
            intstart = intervalLow getInterval + toInteger takable
            intend = intstart + fromIntegral (B.length uploadable) - 1
        --
        -- It shouldn't be possible to get anything except a PartData here due
        -- to the way our measurement is defined.
        (FT.EmptyR, otherPart :< _) ->
            error $ "EmptyR case of findUploadableChunk expected PartData, got: " <> show otherPart
        --
        -- Predicate flipped after a while: extra the matching element and
        -- merge the remaining left and right trees.
        -- XXX Do something with `before`
        (lefties :> before, PartData{getInterval, getShareData} :< righties) ->
            ( Just (UploadInfo partNum getShareData)
            , (lefties |> PartUploading partNum getInterval) >< righties
            )
          where
            partNum = assignNumber getInterval

        --
        -- It shouldn't be possible to get anything except a PartData
        -- here due to the way our measurement is defined.
        (_ :> _, otherPart :< _) ->
            error $ "non-empty case of findUploadableChunk expected PartData, got: " <> show otherPart

-- ......111122222222......
-- 44444411UUUUUUUU22333333
-- 123456781234567812345678

highestPartNumber :: UploadTreeMeasure backend -> PartNumber
highestPartNumber = undefined -- offsetToPartNumber . high . coveringInterval

offsetToPartNumber :: Size -> Size -> Size -> PartNumber
offsetToPartNumber minSize totalSize n = PartNumber $ 1 + n `div` max minSize partSizeForTotalSize
  where
    partSizeForTotalSize = totalSize `div` 10_000

partNumberToInterval :: PartNumber -> Interval
partNumberToInterval (PartNumber n) = Interval ((n - 1) * 5_000_000) (n * 5_000_000 - 1)

replace :: IsBackend backend => PartNumber -> Part backend a -> UploadTree backend a -> Maybe (UploadTree backend a)
replace partNum newPart t@UploadTree{uploadTree} =
    case (left, right') of
        (lefties, target :< righties) ->
            -- The new part might completely or only partially overlap with the
            -- part being replaced.  If it partially overlaps, leave behind
            -- whatever parts of target aren't covered by the new part.
            -- Otherwise, drop the new part in place of the target.
            --
            -- XXX Deal with the partial overlaps.  Maybe by making sure all
            -- nodes in the tree are actually part boundary aligned?
            Just $ t{uploadTree = (lefties |> newPart) >< righties}
        _ ->
            -- Any other result means we didn't find a node relate to the
            -- PartNumber in the way we wanted.  Abandon the replacement
            -- attempt.  Ideally this case is impossible ...
            Nothing
  where
    (left, right) = FT.split (\p -> highestPartNumber p <= partNum) uploadTree
    right' = FT.viewl right

-- XXX Prevent overlaps
insert :: IsBackend backend => Part backend a -> UploadTree backend a -> UploadTree backend a
insert p t@UploadTree{uploadTree} =
    t{uploadTree = tree'}
  where
    -- Our predicate should flip from "false" -> "true" _on_ the
    -- element we want to insert before
    position m = low (getInterval p) <= high (coveringInterval m)

    (left, right) = FT.split position uploadTree

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

-- -- What Monoid / Measured works for our use-case?
-- --
-- --  Find contiguous intervals greater than some minimum size
-- --  Look up data associated with an interval
-- --  Maybe merge contiguous intervals
