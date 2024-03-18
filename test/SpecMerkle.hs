{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module SpecMerkle (
    tests,
) where

import Crypto.Hash (hash)
import Data.Binary (decodeOrFail, encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.List (
    sort,
 )
import Data.Maybe (
    isJust,
 )
import Data.String (fromString)
import Generators (digests, merkleTrees)
import Hedgehog (
    MonadTest,
    Property,
    annotateShow,
    assert,
    diff,
    failure,
    forAll,
    label,
    property,
    tripping,
 )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Tahoe.CHK.Crypto (
    taggedHash',
 )
import Tahoe.CHK.Merkle (
    Direction (..),
    MerkleTree (MerkleLeaf, MerkleNode),
    breadthFirstList,
    buildTreeOutOfAllTheNodes,
    checkMerkleProof,
    emptyLeafHash,
    height,
    leafNumberToNode,
    leafNumberToNodeNumber,
    makeTree,
    mapTree,
    merklePath,
    merkleProof,
    neededHashes,
    pairHash,
    rootHash,
    size,
 )
import Tahoe.CHK.SHA256d (Digest' (Digest'), SHA256d, toBytes)
import Test.Tasty (
    TestTree,
    testGroup,
 )
import Test.Tasty.HUnit (
    assertBool,
    assertEqual,
    testCase,
 )
import Test.Tasty.Hedgehog (testProperty)

sha256d :: B.ByteString -> Digest' SHA256d
sha256d = Digest' . hash

sha256dBytes :: B.ByteString -> B.ByteString
sha256dBytes = toBytes . sha256d

pairSHA256d :: B.ByteString -> B.ByteString -> Digest' SHA256d
pairSHA256d = pairHash

tests :: TestTree
tests =
    testGroup
        "Merkle"
        [ testCase "pairHash" $
            assertEqual
                "simple test vector"
                "mnp3f5b64ghvupq3u7zt76d7zp6nvhhv5kmflt2iporigi5el57q"
                (pairHash @SHA256d "abc" "xyz")
        , testCase "emptyLeafHash" $
            assertEqual
                "simple test vector"
                "t3kza5vwx3tlowdemmgdyigp62ju57qduyfh7uulnfkc7mj2ncrq"
                (emptyLeafHash @SHA256d 3)
        , testCase "two leaf tree" $
            assertEqual
                "root hash is leaf pair hash"
                (Just "mdopl4owpdikpfqxeigeqlrqlzbecz42fslvszbhsa5kdsxb4xpa")
                (rootHash <$> makeTree (sha256d <$> ["abc", "xyz"]))
        , testCase "three leaf tree" $
            assertEqual
                "root hash of three leaf tree includes empty node hash"
                (Just $ pairSHA256d (toBytes $ pairSHA256d (sha256dBytes "abc") (sha256dBytes "xyz")) (toBytes $ pairSHA256d (sha256dBytes "mno") $ toBytes (emptyLeafHash @SHA256d 3)))
                (rootHash <$> makeTree (sha256d <$> ["abc", "xyz", "mno"]))
        , testCase "empty tree" $
            assertEqual
                "empty list results in no tree"
                Nothing
                (makeTree @SHA256d [])
        , testCase "tiny tree" $
            assertEqual
                "a two leaf tree can be constructed"
                (Just (MerkleNode "rja6pertnjkplyb36vhkfmjdcjyrwyavc77nrfgnanxftv2d7tyq" (MerkleLeaf (sha256d "bar")) (MerkleLeaf (sha256d "baz"))))
                (makeTree (sha256d <$> ["bar", "baz"]))
        , testCase "make 6 leaf tree" $
            assertBool "it can be made" $
                isJust (makeTestTree 6)
        , testCase "breadth first traversal (small)" $
            assertEqual
                "tree with one leaf"
                (Just 1)
                (length . breadthFirstList <$> makeTestTree 1)
        , testCase "breadth first traversal (big)" $
            assertEqual
                "tree with 1024 leaves"
                (Just (1024 * 2 - 1))
                (length . breadthFirstList <$> makeTestTree 1024)
        , testCase "show it" $ do
            print $ makeTestTree 2
            return ()
        , testCase "well-known tree" $
            assertEqual
                "built tree does not equal well-known correct tree"
                (makeTestTree 3)
                ( Just $
                    MerkleNode
                        "vxuqudnucceja4pqkdqy5txapagxubm5moupzqywkbg2jrjkaola"
                        ( MerkleNode
                            "weycjri4jlcaunca2jyx2kr7sbtb7qdriog3f26g5jpc5awfeazq"
                            (MerkleLeaf "esd34nbzri75l3j2vwetpk3dvlvsxstkbaktomonrulpks3df3sq")
                            (MerkleLeaf "jkxbwa2tppyfax35o72tbjecxvaa4xphma6zbyfbkkku3ed2657a")
                        )
                        ( MerkleNode
                            "5ovy3g2wwjnxoqtja4licckxkbqjef4xsjtclk6gxnsl66kvow6a"
                            (MerkleLeaf "wfisavaqgab2raihe7dld2qjps4rtxyiubgfs5enziokey2msjwa")
                            (MerkleLeaf "t3kza5vwx3tlowdemmgdyigp62ju57qduyfh7uulnfkc7mj2ncrq")
                        )
                )
        , testCase "neededHashes test vectors" $
            let Just tree = makeTestTree 8
                needed = (sort . map fst <$>) . neededHashes tree
             in do
                    assertEqual "test vector 1" (Just [2 :: Int, 4, 8]) (needed 0)
                    assertEqual "test vector 2" (Just [2, 4, 7]) (needed 1)
                    assertEqual "test vector 3" (Just [1, 5, 13]) (needed 7)
        , testProperty "all paths same length for merkleProof" prop_merkleProof_length
        , testProperty "all internal nodes have the correct hash" prop_makeTree_hashes
        , testProperty "all merkleProofs prove what they ought" spec_merkleProof_nodeNumbers
        , testProperty "all merkleProofs prove what they ought" spec_merkleProof_hashes
        , testProperty "all merkle paths have a consistent length" spec_merklePath_length
        , testProperty "node numbering round trips through the converters" spec_numberConversion_tripping
        , testProperty "merkle tree block construction" spec_merkleFromRows
        , testProperty "invalid merkle trees fail" spec_invalidMerkle
        , testProperty "checkMerkleProof accepts all merkleProof results" prop_checkMerkleProof_accept
        , testProperty "checkMerkleProof rejects proofs that do not prove the inclusion of the given leaf hash" prop_checkMerkleProof_reject
        , testProperty "merkle trees round-trip through encode / decode" prop_binary_tripping
        ]

heightLabel :: MonadTest m => MerkleTree value hash -> m ()
heightLabel = label . fromString . ("tree height == " <>) . show . height

prop_checkMerkleProof_accept :: Property
prop_checkMerkleProof_accept = property $ do
    someTree <- forAll $ merkleTrees @SHA256d (Range.linear 1 256)
    heightLabel someTree
    someLeafNum <- forAll $ Gen.integral (Range.linear 0 $ height someTree - 1)
    let Just proof = merkleProof someTree someLeafNum
        Just someLeaf = leafNumberToNode someTree someLeafNum
    annotateShow proof
    annotateShow someLeaf
    diff True (==) (checkMerkleProof proof (rootHash someTree) (rootHash someLeaf))

prop_checkMerkleProof_reject :: Property
prop_checkMerkleProof_reject = property $ do
    someTree <- forAll $ merkleTrees @SHA256d (Range.linear 1 256)
    heightLabel someTree
    someLeafNum <- forAll $ Gen.integral (Range.linear 0 $ height someTree - 1)
    let Just proof = merkleProof someTree someLeafNum
        Just someLeaf = leafNumberToNode someTree someLeafNum
    annotateShow proof
    annotateShow someLeaf
    anotherHash <- forAll $ Gen.filterT (/= rootHash someLeaf) digests
    diff False (==) $ checkMerkleProof proof (rootHash someTree) anotherHash

prop_binary_tripping :: Property
prop_binary_tripping = property $ do
    someTree <- forAll $ merkleTrees @SHA256d (Range.linear 1 256)
    let third (_, _, x) = x
    tripping someTree encode ((third <$>) . decodeOrFail)

prop_merkleProof_length :: Property
prop_merkleProof_length = property $ do
    someTree <- forAll $ merkleTrees @SHA256d (Range.linear 1 256)
    someLeaf <- forAll $ Gen.integral (Range.linear 0 $ height someTree - 1)
    diff (Just $ height someTree - 1) (==) (length <$> merkleProof someTree someLeaf)

prop_makeTree_hashes :: Property
prop_makeTree_hashes = property $ do
    someTree <- forAll $ merkleTrees @SHA256d (Range.linear 1 256)
    assert (and $ mapTree checkMerkleProperty someTree)
  where
    checkMerkleProperty (MerkleLeaf _) = True
    checkMerkleProperty (MerkleNode h l r) = h == pairHash (toBytes $ rootHash l) (toBytes $ rootHash r)

{- | Convert a set of directions to a node to that node's number.  The first
 argument is the node number of the root node from which to follow the
 directions.  For the "true" root of the tree, use 1.
-}
pathToNumber :: Int -> [Direction] -> Int
pathToNumber rootNum [] = rootNum
pathToNumber rootNum (d : ds) = pathToNumber childNum ds
  where
    childNum = case d of
        TurnLeft -> rootNum * 2
        TurnRight -> rootNum * 2 + 1

{- | Convert a set of directions to a node to the numbers of the nodes on the
 proof path to that node.  These are the numbers of the nodes that are
 _siblings_ to nodes on the given path.
-}
proofPathNodes :: Int -> [Direction] -> [Int]
proofPathNodes _ [] = []
proofPathNodes rootNum (d : ds) = siblingNum : proofPathNodes childNum ds
  where
    childNum = case d of
        TurnLeft -> rootNum * 2
        TurnRight -> rootNum * 2 + 1

    siblingNum = case d of
        TurnLeft -> rootNum * 2 + 1
        TurnRight -> rootNum * 2

{- | merkleProof returns a list of tuples where each tuple gives a node number
 and the hash belonging to that node.
-}
spec_merkleProof_hashes :: Property
spec_merkleProof_hashes = property $ do
    someTree <- forAll $ merkleTrees @SHA256d (Range.linear 1 256)
    someLeafNum <- forAll $ Gen.integral (Range.linear 0 $ height someTree - 1)

    let proof = merkleProof someTree someLeafNum
        -- Brute force search the tree for a matching node.
        getNode :: Int -> MerkleTree value hash -> Int -> [MerkleTree value hash]
        getNode thisNodeNum n@(MerkleLeaf _) targetNodeNum
            | thisNodeNum == targetNodeNum = [n]
            | otherwise = []
        getNode thisNodeNum n@(MerkleNode _ left right) targetNodeNum
            | thisNodeNum == targetNodeNum = [n]
            | otherwise =
                getNode (thisNodeNum * 2) left targetNodeNum
                    ++ getNode (thisNodeNum * 2 + 1) right targetNodeNum

    annotateShow proof

    case proof of
        Nothing -> failure
        Just proof' -> diff (map snd proof') (==) (map (rootHash . head . getNode 1 someTree . fst) proof')

{- | merkleProof returns a list of tuples where each tuple contains a node
 number which is a sibling of a node on the path to a given leaf.
-}
spec_merkleProof_nodeNumbers :: Property
spec_merkleProof_nodeNumbers = property $ do
    someTree <- forAll $ merkleTrees @SHA256d (Range.linear 1 256)

    -- Choose an arbitrary path through the tree.
    somePath <-
        forAll $
            Gen.list (Range.singleton $ height someTree - 1) $
                Gen.element [TurnLeft, TurnRight]

    let -- Identify the node at the end of the path
        nodeNum = pathToNumber 1 somePath
        leafNum = nodeNumberToLeafNumber someTree nodeNum

        -- Determine the proof path.  It consists of the node numbers of the
        -- siblings of the nodes on the merkle path.
        someProof = proofPathNodes 1 somePath

    annotateShow nodeNum
    annotateShow leafNum

    -- The computed proof path has node numbers which match the proof path node
    -- numbers we computed above.
    diff (map fst <$> merkleProof someTree leafNum) (==) (Just someProof)

spec_numberConversion_tripping :: Property
spec_numberConversion_tripping = property $ do
    someTree <- forAll $ merkleTrees @SHA256d (Range.linear 1 256)
    someNum <- forAll $ Gen.integral (Range.linear 1 $ size someTree - 1)
    tripping someNum (leafNumberToNodeNumber someTree) (pure . nodeNumberToLeafNumber someTree :: Int -> Maybe Int)

-- | We can build a Merkle tree from its flattened form
spec_merkleFromRows :: Property
spec_merkleFromRows = property $ do
    validTree <- forAll $ merkleTrees (Range.linear 1 256)
    let nodes = breadthFirstList validTree

    let (Just alleged) = buildTreeOutOfAllTheNodes @SHA256d nodes
    diff alleged (==) validTree

-- | Invalid flattened trees produce errors
spec_invalidMerkle :: Property
spec_invalidMerkle = property $ do
    validTree <- forAll $ merkleTrees (Range.linear 1 256)
    -- it's a valid list, missing one of the elements
    let nodes = tail (breadthFirstList validTree)

    let maybeTree = buildTreeOutOfAllTheNodes @SHA256d nodes
    diff maybeTree (==) Nothing

-- | The length of all merkle paths equals one less than the given height.
spec_merklePath_length :: Property
spec_merklePath_length = property $ do
    height' <- forAll $ Gen.integral (Range.linear 2 16)
    leafNum <- forAll $ Gen.integral (Range.linear 0 (height' - 1))
    let path = merklePath height' leafNum
    diff (length path) (==) (height' - 1)

makeTestTree :: Int -> Maybe (MerkleTree B.ByteString SHA256d)
makeTestTree numleaves = makeTree $ taggedHash' "tag" . C8.pack . show <$> [0 .. numleaves - 1]

nodeNumberToLeafNumber :: MerkleTree value hash -> Int -> Int
nodeNumberToLeafNumber tree nodeNum = nodeNum - 1 - size tree `div` 2
