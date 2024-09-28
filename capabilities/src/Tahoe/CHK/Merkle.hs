{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tahoe.CHK.Merkle (
    MerkleTree (MerkleNode, MerkleLeaf),
    Direction (..),
    leafNumberToNode,
    leafNumberToNodeNumber,
    breadthFirstList,
    merklePathLengthForSize,
    heightForLeafCount,
    makeTree,
    makeTreePartial,
    merkleProof,
    checkMerkleProof,
    neededHashes,
    firstLeafNum,
    rootHash,
    pairHash,
    emptyLeafHash,
    size,
    height,
    mapTree,
    merklePath,
    leafHashes,
    -- exported for testing in ghci
    treeFromRows,
    buildTreeOutOfAllTheNodes,
    dumpTree,
) where

import Control.Monad ((>=>))
import Crypto.Hash (HashAlgorithm (hashDigestSize), digestFromByteString)
import Data.Binary (Binary (get, put))
import Data.Binary.Get (getRemainingLazyByteString)
import Data.Binary.Put (putByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import Data.Function (on)
import Data.List.HT (
    padLeft,
 )
import Data.Text (Text)
import qualified Data.Text as T
import Data.TreeDiff.Class (ToExpr (..))
import Data.Tuple.HT (
    mapFst,
 )
import GHC.Generics (Generic)
import Tahoe.CHK.Crypto (
    taggedHash',
    taggedPairHash',
    toBytes,
 )
import Tahoe.CHK.SHA256d (Digest' (..), SHA256d)
import Tahoe.Util (
    chunkedBy,
    nextPowerOf,
    toBinary,
 )

{- | A Merkle tree parameterized on a value type and hash algorithm.  The value
 type is phantom and is intended to help avoid mixing up opaque hashes from
 Merkle trees used for different purposes.
-}
data MerkleTree value hash
    = MerkleLeaf (Digest' hash)
    | MerkleNode (Digest' hash) (MerkleTree value hash) (MerkleTree value hash)
    deriving (Eq, Ord, Generic, ToExpr)

-- | Count the number of nodes in a tree.
size :: MerkleTree v a -> Int
size = sum . mapTree (const 1)

-- | Measure the height of a tree.
height :: MerkleTree v a -> Int
height (MerkleLeaf _) = 1
height (MerkleNode _ left _) = 1 + height left

{- | Compute the minimum height for a tree that can hold the given number of
 leaves.
-}
heightForLeafCount :: Integral n => n -> Int
heightForLeafCount num = ceiling $ logBase (2 :: Float) (fromIntegral num)

mapTree :: (MerkleTree v a -> b) -> MerkleTree v a -> [b]
mapTree f l@(MerkleLeaf _) = [f l]
mapTree f n@(MerkleNode _ left right) = f n : mapTree f left ++ mapTree f right

instance (HashAlgorithm hash) => Show (MerkleTree value hash) where
    show (MerkleLeaf value) = "MerkleLeaf " <> show value
    show (MerkleNode value left right) =
        T.unpack $
            T.concat
                [ "MerkleNode " :: T.Text
                , T.pack $ show value
                , " ("
                , T.pack $ show left
                , ")"
                , " ("
                , T.pack $ show right
                , ")"
                ]

emptyLeafHash :: HashAlgorithm hash => Int -> Digest' hash
emptyLeafHash = taggedHash' "Merkle tree empty leaf" . C8.pack . show

pairHash :: HashAlgorithm a => B.ByteString -> B.ByteString -> Digest' a
pairHash = taggedPairHash' "Merkle tree internal node"

rootHash :: MerkleTree v a -> Digest' a
rootHash (MerkleLeaf value) = value
rootHash (MerkleNode value _ _) = value

-- Like makeTree but error on empty list
makeTreePartial :: HashAlgorithm hash => [Digest' hash] -> MerkleTree value hash
makeTreePartial = unJust . makeTree
  where
    unJust Nothing = error "Merkle.makeTreePartial failed to make a tree"
    unJust (Just t) = t

-- Make a merkle tree for the given values.  Extra values are generated to
-- fill the tree if necessary.  The given values are the values of the leaf
-- nodes.
makeTree :: forall hash value. HashAlgorithm hash => [Digest' hash] -> Maybe (MerkleTree value hash)
makeTree [] = Nothing
makeTree leaves =
    Just $ makeTree' (pad leaves)
  where
    -- Pad the leaves out to the next power of two so the tree is full.
    pad :: [Digest' hash] -> [Digest' hash]
    pad leaves' = leaves' ++ padding (length leaves')

    -- Create the padding for the pad function.  The number of leaves in the
    -- tree must be a power of 2 (a height zero tree has 2 ^ 0 leaves, a
    -- height one tree has 2 ^ 1 leaves, etc) so compute a number of empty
    -- leaves that when added to the non-empty leaves gives us a power of 2.
    -- This could be none if we happened to already have a number of leaves
    -- that is a power of 2.
    --
    -- This function assumes that the number of non-empty leaves is at least
    -- half the number of total leaves.  If it is fewer it will create less
    -- padding than necessary.  This should be reasonable since if there fewer
    -- leaves then a smaller tree could hold them all.
    padding :: Int -> [Digest' hash]
    padding numLeaves = emptyLeafHash <$> [numLeaves .. nextPowerOf 2 numLeaves - 1]

    -- Turn a length-of-power-of-2 list into a tree
    makeTree' :: [Digest' hash] -> MerkleTree value hash
    makeTree' [x] = MerkleLeaf x
    makeTree' xs =
        makeNode (makeTree' left) (makeTree' right)
      where
        (left, right) = splitAt (length xs `div` 2) xs

    -- Make a parent node referencing two given child nodes, calculating the
    -- parent node's hash in the process.
    makeNode :: MerkleTree value hash -> MerkleTree value hash -> MerkleTree value hash
    makeNode left right = MerkleNode ((pairHash `on` toBytes . rootHash) left right) left right

-- | Represent a direction to take when walking down a binary tree.
data Direction = TurnLeft | TurnRight deriving (Show, Ord, Eq)

{- | Return a list of tuples of node numbers and corresponding merkle hashes.
 The node numbers correspond to a numbering of the nodes in the tree where the
 root node is numbered 1, each node's left child is the node's number times
 two, and the node's right child is the node's number times two plus one.
-}
merkleProof :: MerkleTree v a -> Int -> Maybe [(Int, Digest' a)]
merkleProof tree targetLeaf = merkleProof' 1 tree $ merklePath (height tree) targetLeaf

{- | Check a merkle proof for validity.  The proof is valid if it represents
 the correct hash chain from the given leaf to the given root.
-}
checkMerkleProof ::
    forall n hash.
    (Integral n, HashAlgorithm hash) =>
    -- | The proof to check.
    [(n, Digest' hash)] ->
    -- | The root hash of the merkle tree against which to check the proof.
    Digest' hash ->
    -- | The leaf hash against which to check the proof.
    Digest' hash ->
    -- | True if the proof checks out, False otherwise.
    Bool
checkMerkleProof proof expectedRootHash leafHash = expectedRootHash == check proof
  where
    check :: [(n, Digest' hash)] -> Digest' hash
    check [] = leafHash
    check ((nodeNum, nodeHash) : more)
        | even nodeNum = pairHashD nodeHash (check more)
        | otherwise = pairHashD (check more) nodeHash
      where
        pairHashD = pairHash `on` toBytes

{- | Compute the path to a leaf from the root of a merkle tree of a certain
 height.
-}
merklePath :: Int -> Int -> [Direction]
merklePath height' leafNum = padLeft TurnLeft (height' - 1) (toBinary TurnLeft TurnRight leafNum)

-- | Compute the length of a merkle path through a tree of the given height.
merklePathLengthForSize :: Int -> Int
merklePathLengthForSize size' = ceiling . logBase (2 :: Double) . fromIntegral $ nextPowerOf 2 size'

-- Convert a tree to a breadth-first list of its hash values.
breadthFirstList :: forall v a. MerkleTree v a -> [Digest' a]
breadthFirstList tree = traverse' [tree]
  where
    traverse' :: [MerkleTree v a] -> [Digest' a]
    traverse' [] = []
    traverse' trees =
        [rootHash tree' | tree' <- trees] ++ traverse' (concat [children tree'' | tree'' <- trees])

    children (MerkleLeaf _) = []
    children (MerkleNode _ left right) = [left, right]

{- | Construct Just a merkle proof along the pre-computed path or Nothing if
 the path runs past the leaves of the tree.
-}
merkleProof' :: Int -> MerkleTree v a -> [Direction] -> Maybe [(Int, Digest' a)]
merkleProof' _ _ [] = Just []
merkleProof' thisNodeNum (MerkleNode _ left right) (d : ds) =
    case d of
        TurnLeft ->
            ((rightChildNum, rootHash right) :) <$> merkleProof' leftChildNum left ds
        TurnRight ->
            ((leftChildNum, rootHash left) :) <$> merkleProof' rightChildNum right ds
  where
    leftChildNum = thisNodeNum * 2
    rightChildNum = thisNodeNum * 2 + 1
merkleProof' _ (MerkleLeaf _) ds = error $ show ds

{- | Translate a leaf number to a node number.  Leaf numbers are zero indexed
 and identify leaves of a tree from left to right.  Node numbers are one
 indexed and identify nodes of a tree from top to bottom, left to right.
-}
leafNumberToNodeNumber :: MerkleTree v a -> Int -> Int
leafNumberToNodeNumber tree leafNum = 1 + leafNum + firstLeafNum tree

{- | Get a leaf node by its leaf number, if possible.  Leaf numbers are zero indexed
  and identify leaves of a tree from left to right.
-}
leafNumberToNode :: MerkleTree v a -> Int -> Maybe (MerkleTree v a)
leafNumberToNode tree leafNum = nodeAtPath tree path
  where
    path = merklePath (height tree) leafNum

    nodeAtPath node [] = Just node
    nodeAtPath (MerkleNode _ left _) (TurnLeft : ds) = nodeAtPath left ds
    nodeAtPath (MerkleNode _ _ right) (TurnRight : ds) = nodeAtPath right ds
    nodeAtPath (MerkleLeaf _) _ = Nothing

{- | Get a merkle proof but re-number the node numbers to be zero-indexed
 instead of one-indexed.
-}
neededHashes :: MerkleTree v a -> Int -> Maybe [(Int, Digest' a)]
neededHashes tree = fmap (map $ mapFst (subtract 1)) . merkleProof tree

{- | Determine the smallest index into the breadth first list for the given
 tree where a leaf may be found.
-}
firstLeafNum :: MerkleTree v a -> Int
firstLeafNum tree = size tree `div` 2

{- | Serialize a MerkleTree to bytes by concatenating all of the leaf hashes
 left to right.

 This serialization includes no framing so the only thing we can do is
 consume all available input.  Use this instance with `isolate` and bring
 your own framing mechanism to determine how many bytes to process.
-}
instance (Show hash, HashAlgorithm hash) => Binary (MerkleTree v hash) where
    put = putByteString . B.concat . fmap toBytes . breadthFirstList
    get =
        getRemainingLazyByteString
            >>= maybe (fail "could not construct MerkleTree") pure
                . (mapM (fmap Digest' . digestFromByteString) >=> buildTreeOutOfAllTheNodes)
                . chunkedBy (hashDigestSize (undefined :: SHA256d))
                . LBS.toStrict

-- | Get a list of all of the leaf hashes of a tree from left to right.
leafHashes :: MerkleTree v a -> [Digest' a]
leafHashes (MerkleLeaf h) = [h]
leafHashes (MerkleNode _ l r) = leafHashes l <> leafHashes r

{- | Make a merkle tree out of a flat list of all nodes (start from
 root, then first two children, etc .. [0, 1, 2] is a two-layer
 tree, [0, 1, 2, 3, 4, 5, 6] is three-layer, etc
-}
buildTreeOutOfAllTheNodes :: (Show hash, HashAlgorithm hash) => [Digest' hash] -> Maybe (MerkleTree value hash)
buildTreeOutOfAllTheNodes nodes
    | validMerkleSize nodes = Just (head (treeFromRows [] (clumpRows powersOfTwo nodes)))
    | otherwise = Nothing

{- | Increasing consecutive powers of 2 from 2 ^ 0 to the maximum value
 representable in `Int`.
-}
powersOfTwo :: [Int]
powersOfTwo = (2 ^) <$> [0 :: Int .. 62]

{- | Determine whether a list of nodes is a possible representation of a
 merkle tree.

 It is possible if the number of elements in the list is one less than a
 positive power of 2.
-}
validMerkleSize :: [a] -> Bool
validMerkleSize nodes =
    head (dropWhile (< size') (tail powersOfTwo)) == size'
  where
    size' = length nodes + 1

{- | Reorganize a flat list of merkle tree node values into a list of lists of
 merkle tree node values.  Each inner list gives the values from left to right
 at a particular height in the tree.  The head of the outer list gives the
 leaves.
-}
clumpRows ::
    -- | The numbers of elements of the flat list to take to make this (the
    -- head) and subsequent (the tail) clumps.
    [Int] ->
    -- | The values of the nodes themselves.
    [a] ->
    [[a]]
clumpRows _ [] = []
clumpRows [] _ = error "Ran out of clump lengths (too many nodes!)"
clumpRows (p : ps) rows = clumpRows ps (drop p rows) ++ [take p rows]

-- | Given some children
treeFromRows ::
    (Show hash, HashAlgorithm hash) =>
    -- | Some children to attach to a list of nodes representing the next
    -- shallowest level of the tree.
    [MerkleTree value hash] ->
    -- | The values of the nodes to create at the next shallowest level of the
    -- tree.
    [[Digest' hash]] ->
    -- | The nodes forming the shallowest level of the tree.  If we built a
    -- full tree, there will be exactly one node here.
    [MerkleTree value hash]
-- if we've processed nothing yet, we're on the "all leafs" children row
treeFromRows [] (children : rest) = treeFromRows (MerkleLeaf <$> children) rest
-- if we're out of other stuff then we're done
treeFromRows children [] = children
-- with only a single thing in the "rest", we're at the root
treeFromRows [left, right] [[root]] = [MerkleNode root left right]
-- this recursion is harder to think about: we want to "collect" done
-- stuff from the first argument and build it up into a tree. kind of.
treeFromRows (left : right : children) (row : rest) = treeFromRows (mTree (left : right : children) row) rest
treeFromRows x y = error $ "treeFromRows not sure what to do with " <> show x <> " " <> show y

-- this does the "second recursion"; see above -- building out a row
-- of parents from children + parent node content
mTree :: HashAlgorithm hash => [MerkleTree value hash] -> [Digest' hash] -> [MerkleTree value hash]
mTree [left, right] [head'] = [MerkleNode head' left right]
mTree (left : right : more) row = MerkleNode (head row) left right : mTree more (tail row)
mTree x y = error $ "mTree not sure what to do with " <> show x <> " " <> show y

dumpTree :: HashAlgorithm hash => MerkleTree value hash -> [Text]
dumpTree (MerkleLeaf hash) = ["Leaf " <> (T.pack . show) hash]
dumpTree (MerkleNode hash left right) =
    ("Node " <> (T.pack . show) hash) : indent (dumpTree left) ++ indent (dumpTree right)
  where
    indent = fmap ("   \\" <>)
