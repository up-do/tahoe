{-# LANGUAGE MultiParamTypeClasses #-}

module Tahoe.Storage.Backend.Internal.BufferedUploadTree where

import qualified Amazonka.S3 as S3
import qualified Data.ByteString as B
import Data.FingerTree (ViewL ((:<)), ViewR ((:>)), (<|), (><), (|>))
import qualified Data.FingerTree as FT
import Data.Maybe (fromMaybe)

data Interval = Interval Int Int deriving (Eq, Ord, Show)

low :: Interval -> Int
low (Interval l _) = l

high :: Interval -> Int
high (Interval _ h) = h

data ContiguousSize = NotContiguous | ContiguousSize Interval deriving (Eq, Ord, Show)

atLeast :: ContiguousSize -> Int -> Bool
atLeast NotContiguous n = 0 >= n
atLeast (ContiguousSize i) n = intervalSize i >= n

instance Semigroup ContiguousSize where
    NotContiguous <> c = c
    c <> NotContiguous = c
    ContiguousSize l@(Interval al ah) <> ContiguousSize r@(Interval bl bh) =
        if succ ah == bl
            then ContiguousSize (Interval al bh)
            else ContiguousSize $ if intervalSize l >= intervalSize r then l else r

instance Monoid ContiguousSize where
    mempty = NotContiguous

intervalSize :: Interval -> Int
intervalSize (Interval l h) = h - l

instance Semigroup Interval where
    -- As an optimization, we can ignore `lowB` because we know left starts
    -- smaller than right.
    Interval lowA highA <> Interval _lowB highB = Interval lowA (max highA highB)

instance Monoid Interval where
    mempty = Interval 0 0

data Part
    = PartData {getInterval :: Interval, getShareData :: B.ByteString}
    | PartUploading {getPartNumber :: PartNumber, getInterval :: Interval}
    | PartUploaded {getPartNumber :: PartNumber, getInterval :: Interval, getPartResponse :: S3.UploadPartResponse}
    deriving (Eq, Show)

newtype PartNumber = PartNumber Int
    deriving newtype (Eq)
    deriving (Show)

data UploadInfo = UploadInfo PartNumber B.ByteString

data UploadTreeMeasure = UploadTreeMeasure
    { coveringInterval :: Interval
    , uploadableBytes :: Int
    , fullyUploaded :: Bool
    }
    deriving (Eq, Show)

instance Semigroup UploadTreeMeasure where
    (UploadTreeMeasure aInt aContig aUp) <> (UploadTreeMeasure bInt bContig bUp) = UploadTreeMeasure (aInt <> bInt) (max aContig bContig) (aUp && bUp)

instance Monoid UploadTreeMeasure where
    mempty = UploadTreeMeasure mempty 0 True

instance FT.Measured UploadTreeMeasure Part where
    measure PartData{getInterval} = UploadTreeMeasure getInterval ((high getInterval) - (low getInterval) + 1) False
    measure PartUploading{getInterval} = UploadTreeMeasure getInterval 0 False
    measure PartUploaded{getInterval} = UploadTreeMeasure getInterval 0 True

type UploadTree = FT.FingerTree UploadTreeMeasure Part

findUploadableChunk :: (Interval -> PartNumber) -> UploadTree -> Int -> (Maybe UploadInfo, UploadTree)
findUploadableChunk assignNumber tree minSize =
    (upload, tree')
  where
    position m = uploadableBytes m >= minSize
    (left, right) = FT.split position tree

    (upload, tree') = case (FT.viewr left, FT.viewl right) of
        --
        -- If we traverse the whole tree (even if only because the tree was
        -- empty) without the predicate flipping, nothing is uploadable
        (_, FT.EmptyL) -> (Nothing, tree)
        --
        -- Predicate flipped immediately: whole tree is uploadable
        (FT.EmptyR, (PartData interval bytes) :< righties) ->
            ( Just (UploadInfo partNum bytes)
            , PartUploading partNum interval <| righties
            )
          where
            partNum = assignNumber interval

        --
        -- It shouldn't be possible to get anything except a PartData here due
        -- to the way our measurement is defined.
        (FT.EmptyR, otherPart :< _) ->
            error $ "EmptyR case of findUploadableChunk expected PartData, got: " <> show otherPart
        --
        -- Predicate flipped after a while: extra the matching element and
        -- merge the remaining left and right trees.
        -- XXX Do something with `before`
        (lefties :> before, (PartData interval bytes) :< righties) ->
            ( Just (UploadInfo partNum bytes)
            , (lefties |> PartUploading partNum interval) >< righties
            )
          where
            partNum = assignNumber interval

        --
        -- It shouldn't be possible to get anything except a PartData
        -- here due to the way our measurement is defined.
        (_ :> _, otherPart :< _) ->
            error $ "non-empty case of findUploadableChunk expected PartData, got: " <> show otherPart

-- XXX Prevent overlaps
insert :: Part -> UploadTree -> UploadTree
insert p tree =
    tree'
  where
    -- Our predicate should flip from "false" -> "true" _on_ the
    -- element we want to insert before
    position m = low (getInterval p) <= high (coveringInterval m)

    (left, right) = FT.split position tree

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
insert p tree =
    tree'
  where
    (left, right) = FT.split ((> low (getInterval p)) . low . coveringInterval) tree

    tree' = left >< FT.singleton p >< right

merge3 :: Part -> Part -> Part -> [Part]
merge3 a b c =
    case merge2 a b of
        Nothing -> case merge2 b c of
            Nothing -> [a, b, c]
            Just bc -> [a, bc]
        Just ab -> case merge2 ab c of
            Nothing -> [ab, c]
            Just abc -> [abc]

merge2 :: Part -> Part -> Maybe Part
merge2 (PartData aInt aData) (PartData bInt bData)
    | succ (high aInt) == low bInt = Just (PartData (aInt <> bInt) (aData <> bData))
    | otherwise = Nothing
merge2 _ _ = Nothing

merge2' :: Part -> Part -> [Part]
merge2' a b = maybe [a, b] (: []) $ merge2 a b

-- -- What Monoid / Measured works for our use-case?
-- --
-- --  Find contiguous intervals greater than some minimum size
-- --  Look up data associated with an interval
-- --  Maybe merge contiguous intervals
