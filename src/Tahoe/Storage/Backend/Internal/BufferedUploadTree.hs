module Tahoe.Storage.Backend.Internal.BufferedUploadTree (empty) where

import qualified Data.ByteString as B
import qualified Data.FingerTree as FT
import Data.List (sortOn)

data BufferedData = BufferedData Int Int B.ByteString

data Interval = Interval Integer Integer
data Contiguous
    = C Integer Integer [Interval] -- start end of all the intervals
    | NotContiguous [Contiguous]

i2c :: Interval -> Contiguous
i2c i@(Interval low high) = C low high [i]

c2i :: Contiguous -> Interval
c2i (C low high _) = Interval low high

intervalSize :: Contiguous -> Integer
intervalSize (C low high _) = high - low

mergeMaybe :: Contiguous -> Contiguous -> Contiguous
mergeMaybe c1@(C s1 s2 sublist1) c2@(C e1 e2 sublist2) = if s2 + 1 == e1 then (C s1 e2 (sublist1 <> sublist2)) else NotContiguous c1 c2
mergeMaybe (NotContiguous cs) (C s1 s2 sublist1) = undefined

newtype GreatestInterval = GreatestInterval [Interval]

instance Semigroup GreatestInterval where
    GreatestInterval a <> GreatestInterval b = GreatestInterval (a <> b)

instance Monoid GreatestInterval where
    mempty = GreatestInterval []

greatestIntervals :: GreatestInterval -> [Contiguous]
greatestIntervals (GreatestInterval []) = []
greatestIntervals (GreatestInterval (i : is)) = sortOn intervalSize (foldr mergeMaybe (i2c i) (i2c <$> is))

empty :: FT.FingerTree GreatestInterval BufferedData
empty = undefined

-- What Monoid / Measured works for our use-case?
--
--  Find contiguous intervals greater than some minimum size
--  Look up data associated with an interval
--  Maybe merge contiguous intervals
