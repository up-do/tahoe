{-# LANGUAGE ScopedTypeVariables #-}

module Tahoe.CHK.Validate where

import Control.Lens (view)
import Crypto.Hash (HashAlgorithm)
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString.Lazy as LB
import Tahoe.CHK.Capability (Verifier, fingerprint)
import Tahoe.CHK.Crypto (blockHash', ciphertextSegmentHash', uriExtensionHash)
import Tahoe.CHK.Merkle (checkMerkleProof, heightForLeafCount, leafHashes, rootHash)
import Tahoe.CHK.SHA256d (Digest', SHA256d)
import Tahoe.CHK.Share (Crypttext, Share (..), blockHashTree, blocks, crypttextHashTree, neededHashes, uriExtension)
import Tahoe.CHK.URIExtension (crypttextRootHash, shareRootHash, totalShares)

{- | Determine the validity of the given share's fingerprint as defined by the
 given capability.
-}
validFingerprint :: Verifier -> Share -> Bool
validFingerprint cap = (== view fingerprint cap) . uriExtensionHash . _uriExtension

{- | True if the root of the crypttext hash tree in the share matches the
 crypttext hash root given in the URI extension block.  False otherwise.
-}
matchingCrypttextHashRoot :: Share -> Bool
matchingCrypttextHashRoot share = inShare == inUEB
  where
    inShare = rootHash . view crypttextHashTree $ share
    inUEB = view (uriExtension . crypttextRootHash) share

{- | True if the share's own hash in the `shareNeededHashes` list equals the
 root of the share's block hash merkle tree, False otherwise.
-}
matchingBlockHashRoot :: Int -> Share -> Bool
matchingBlockHashRoot shareNum share =
    -- We should find exactly one element with a share number matching our
    -- share number and the associated hash should match our hash.  If we find
    -- none or more than one then the share is mis-encoded and we should fail
    -- validation (though maybe we should do so with a distinct error value).
    isMatching
  where
    isMatching =
        checkMatch
            . findOwnHash
            $ view neededHashes share

    checkMatch = ([rootHash (view blockHashTree share)] ==) . map snd

    -- Note that shareNeededHashes contains "node numbers" while our
    -- shareNum is a "leaf number".  So, convert.
    findOwnHash = filter ((== nodeNumber) . fst)

    nodeNumber :: Int
    nodeNumber = toNodeNumber shareNum

    toNodeNumber num = num + (2 ^ treeHeight) - 1
    treeHeight = heightForLeafCount . view (uriExtension . totalShares) $ share

{- | Determine the validity of each of the given shares' "share root hash"
 values with respect to the other shares in the list.
-}
validShareRootHash :: [(Int, Share)] -> [Bool]
validShareRootHash [] = []
validShareRootHash shares@((_, aShare) : _) =
    isValid
  where
    isValid = zipWith (`checkMerkleProof` expected) proofs leafs

    -- You already validated the share fingerprint so the expected share root
    -- hash from the UEB has also been validated and we can use it.  The UEB
    -- is the same for all shares so we can pull this value from an arbitrary
    -- share.
    expected = view (uriExtension . shareRootHash) aShare

    -- Extract the proof for each share in the given list.
    proofs = uncurry oneProof <$> shares

    -- Also extract each share's leaf hash to supply to the proof checker.
    leafs = rootHash . view blockHashTree . snd <$> shares

    oneProof :: Int -> Share -> [(Int, Digest' SHA256d)]
    oneProof shareNum share = fmap (first (+ 1)) proof
      where
        -- The length of the proof equals the height of the tree.
        treeHeight = length (view neededHashes share)

        -- Since inclusion of our block tree root hash is what the proof is
        -- proving we don't want it.  We need to take it out to use our proof
        -- checker.  That means we need to find it.  The "needed hashes" are
        -- labeled by tree _node number_ and our share number is effectively a
        -- _leaf number_ so we need to convert for comparison.

        -- Nodes are numbered consecutively, starting at 0 for the root node
        -- and proceeding left-to-right depth-first.
        firstLeafNum = 2 ^ (treeHeight - 1) - 1
        nodeNum = firstLeafNum + shareNum

        -- The proof is all of the needed hashes except for this share's own
        -- hash which we will feed into the proof checker separately.
        proof = filter ((/= nodeNum) . fst) (first fromIntegral <$> view neededHashes share)

showHashes :: (Show a, Show b) => [(a, b)] -> String
showHashes = unwords . fmap showHash

showHash :: (Show a, Show b) => (a, b) -> String
showHash (n, bs) = unwords [show n, show bs]

{- | Get only and all the blocks from the given share with hashes that match
 the values in the Share's "block hash tree".
-}
shareValidBlocks :: Share -> [Maybe LB.ByteString]
shareValidBlocks share =
    zipWith checkHash (view blocks share) (leafHashes (view blockHashTree share))
  where
    checkHash :: forall hash. HashAlgorithm hash => LB.ByteString -> Digest' hash -> Maybe LB.ByteString
    checkHash bs expected
        | blockHash' (LB.toStrict bs) == expected = Just bs
        | otherwise = Nothing

{- | Compare the hash of one segment to an expected hash value and return
 Nothing if it does not match or Just the segment if it does.
-}
validSegment :: Digest' SHA256d -> Crypttext -> Maybe Crypttext
validSegment expected crypttext
    | ciphertextSegmentHash' crypttext == expected = Just crypttext
    | otherwise = Nothing

-- | Apply @validSegment@ to lists of values.
validSegments :: [Digest' SHA256d] -> [Crypttext] -> [Maybe Crypttext]
validSegments = zipWith validSegment
