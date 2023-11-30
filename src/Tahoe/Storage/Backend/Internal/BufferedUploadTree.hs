{-# LANGUAGE MultiParamTypeClasses #-}

module Tahoe.Storage.Backend.Internal.BufferedUploadTree where

import Debug.Trace (trace)
import qualified Data.ByteString as B
import Data.FingerTree (ViewL ((:<)), ViewR ((:>)), (><), (<|), (|>))
import qualified Data.FingerTree as FT
import Data.Maybe (fromMaybe)

-- newtype ShareDataSize = ShareDataSize {getShareDataSize :: Int}

-- instance Semigroup ShareDataSize where
--     ShareDataSize a <> ShareDataSize b = ShareDataSize (a + b)

-- instance Monoid ShareDataSize where
--     mempty = ShareDataSize 0

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
    | PartUploading {getInterval :: Interval}
    | PartUploaded {getInterval :: Interval}
    deriving (Eq, Show)

-- isUploaded PartData = No
-- isUploaded PartUploading = No
-- isUploaded PartUploaded = Yes

-- data IsUploaded = Yes | No

-- instance Semigroup IsUploaded where
--     Yes <> Yes = Yes
--     _ <> _ = No

-- instance Monoid IsUploaded where
--     mempty = No

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
type PartNumber = Int


findUploadableChunk :: UploadTree -> Int -> (Maybe Part, UploadTree)
findUploadableChunk tree minSize =
    tree'
  where
    position m = uploadableBytes m >= minSize
    (left, right) = FT.split position tree

    tree' = case (FT.viewr left, FT.viewl right) of
        -- If there's nothing around to merge with then we can't upload anything
        (FT.EmptyR, FT.EmptyL) -> (Nothing, tree)
        -- If we traverse the whole tree without the predicate flipping, nothing is uploadable
        (lefties :> before, FT.EmptyL) -> (Nothing, tree)
        -- Predicate flipped immediately: whole tree is uploadable?
        (FT.EmptyR, after :< righties) -> (Just after, (PartUploading (getInterval after)) <| righties)
        -- And if we can get both, try merging both ways.
        (lefties :> before, after :< righties) -> (Just after, (lefties |> (PartUploading (getInterval after))) >< righties)

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

-- data BufferedData = BufferedData Int Int B.ByteString

-- data Interval = Interval Integer Integer
-- data Contiguous
--     = C Integer Integer [Interval] -- start end of all the intervals
--     | NotContiguous [Contiguous]

-- i2c :: Interval -> Contiguous
-- i2c i@(Interval low high) = C low high [i]

-- c2i :: Contiguous -> Interval
-- c2i (C low high _) = Interval low high

-- intervalSize :: Contiguous -> Integer
-- intervalSize (C low high _) = high - low

-- mergeMaybe :: Contiguous -> Contiguous -> Contiguous
-- mergeMaybe c1@(C s1 s2 sublist1) c2@(C e1 e2 sublist2) = if s2 + 1 == e1 then (C s1 e2 (sublist1 <> sublist2)) else NotContiguous c1 c2
-- mergeMaybe (NotContiguous cs) (C s1 s2 sublist1) = undefined

-- newtype GreatestInterval = GreatestInterval [Interval]

-- instance Semigroup GreatestInterval where
--     GreatestInterval a <> GreatestInterval b = GreatestInterval (a <> b)

-- instance Monoid GreatestInterval where
--     mempty = GreatestInterval []

-- greatestIntervals :: GreatestInterval -> [Contiguous]
-- greatestIntervals (GreatestInterval []) = []
-- greatestIntervals (GreatestInterval (i : is)) = sortOn intervalSize (foldr mergeMaybe (i2c i) (i2c <$> is))

-- empty :: FT.FingerTree GreatestInterval BufferedData
-- empty = undefined

-- -- What Monoid / Measured works for our use-case?
-- --
-- --  Find contiguous intervals greater than some minimum size
-- --  Look up data associated with an interval
-- --  Maybe merge contiguous intervals
