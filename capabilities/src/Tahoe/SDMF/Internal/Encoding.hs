{-# LANGUAGE ScopedTypeVariables #-}

{- | Implement the scheme for encoding ciphertext into SDMF shares (and
 decoding it again).
-}
module Tahoe.SDMF.Internal.Encoding where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Crypto.Hash (digestFromByteString)
import Crypto.Random (MonadRandom)
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Word (Word16, Word64, Word8)
import Tahoe.CHK (padCiphertext, zfec, zunfec)
import Tahoe.CHK.Merkle (MerkleTree (MerkleLeaf))
import Tahoe.CHK.SHA256d (Digest' (Digest'), zero)
import Tahoe.SDMF.Internal.Capability (Reader (..), Writer (..), deriveReader)
import Tahoe.SDMF.Internal.Converting (from, tryInto)
import qualified Tahoe.SDMF.Internal.Keys as Keys
import Tahoe.SDMF.Internal.Share (HashChain (HashChain), Share (..))

{- | Given a pre-determined key pair and sequence number, encode some
 ciphertext into a collection of SDMF shares.

 A key pair *uniquely identifies* a "slot" (the storage location for the shares).
 Thus they cannot be re-used for "different" data.  Any shares created with a
 given key pair are part of the same logical data object.
-}
encode :: (MonadFail m, MonadIO m, MonadRandom m) => Keys.KeyPair -> Keys.SDMF_IV -> Word64 -> Word16 -> Word16 -> LB.ByteString -> m ([Share], Writer)
encode keypair iv shareSequenceNumber required total ciphertext = do
    -- Make sure the encoding parameters fit into a Word8
    requiredAsWord8 <- tryInto @Word8 ("must have 0 < required < 255 but required == " <> show required) required
    totalAsWord8 <- tryInto @Word8 ("must have 0 < total < 256 but total == " <> show total) total

    -- And that they make sense together.
    when (required >= total) (fail $ "must have required < total but required == " <> show required <> ", total == " <> show total)

    -- They look okay, we can proceed.
    blocks <- liftIO $ fmap LB.fromStrict <$> zfec (from required) (from total) paddedCiphertext

    -- We know the length won't be negative (doesn't make sense) and we
    -- know all positive values fit into a Word64 so we can do this
    -- conversion safely.  But if it needs to fail for some reason, it
    -- can do so safely.
    dataLength <- tryInto @Word64 "must have 0 <= data length" (LB.length ciphertext)

    -- All segments are the same so we can figure the size from any one
    -- block.  This conversion might fail because of Int64 vs Word64 but
    -- only for truly, truly tremendous share data.
    shareSegmentSize <- tryInto @Word64 "must have segment size < 2^63" (LB.length (head blocks))

    let makeShare' =
            flip $
                makeShare
                    shareSequenceNumber
                    iv
                    requiredAsWord8
                    totalAsWord8
                    dataLength
                    shareSegmentSize
                    (Keys.toVerificationKey keypair)

    let makeShare'' = makeShare' <$> blocks

        resultE :: Either T.Text [Share]
        resultE = (traverse . flip fmap) encryptedPrivateKey makeShare''
    either (fail . T.unpack) pure ((,) <$> resultE <*> cap)
  where
    paddedCiphertext = LB.toStrict $ padCiphertext required ciphertext
    -- We can compute a capability immediately.
    cap = capabilityForKeyPair keypair
    encryptedPrivateKey = flip Keys.encryptSignatureKey (Keys.toSignatureKey keypair) <$> (writerWriteKey <$> cap)

makeShare ::
    Word64 ->
    Keys.SDMF_IV ->
    Word8 ->
    Word8 ->
    Word64 ->
    Word64 ->
    Keys.Verification ->
    B.ByteString ->
    LB.ByteString ->
    Share
makeShare shareSequenceNumber shareIV shareRequiredShares shareTotalShares shareDataLength shareSegmentSize shareVerificationKey shareEncryptedPrivateKey shareData = Share{..}
  where
    shareRootHash = B.replicate 32 0
    shareSignature = B.replicate 32 0 -- XXX Actually compute sig, and is it 32 bytes?
    shareHashChain = HashChain []
    shareBlockHashTree = MerkleLeaf zero -- XXX Real hash here, plus length check

{- | Decode some SDMF shares to recover the original ciphertext.

 TODO: Use the read capability to verify the shares were constructed with
 information from the matching write capability.
-}
decode :: (MonadFail m, MonadIO m) => Reader -> [(Word16, Share)] -> m LB.ByteString
decode _ [] = fail "Cannot decode with no shares"
decode _ s@((_, Share{shareRequiredShares, shareTotalShares, shareDataLength}) : shares)
    -- Make sure we have enough shares.
    | length s < requiredAsInt =
        fail $ "got " <> show (length shares) <> " shares, required " <> show shareRequiredShares
    | otherwise = do
        -- Make sure this implementation can handle the amount of data involved.
        -- Since we use lazy ByteString we're limited to 2^63-1 bytes rather than
        -- 2^64-1 bytes so there are some SDMF shares we can't interpret right
        -- now.
        shareDataLength' <- tryInto @Int64 ("share data length " <> show shareDataLength <> " is beyond maximum supported by this implementation " <> show (maxBound :: Int64)) shareDataLength
        ciphertext <- liftIO $ zunfec requiredAsInt totalAsInt (take requiredAsInt blocks)
        pure . LB.take shareDataLength' . LB.fromStrict $ ciphertext
  where
    blocks = bimap (from @Word16) (LB.toStrict . shareData) <$> s

    requiredAsInt = from shareRequiredShares
    totalAsInt = from shareTotalShares

-- | Compute an SDMF write capability for a given keypair.
capabilityForKeyPair :: Keys.KeyPair -> Either T.Text Writer
capabilityForKeyPair keypair =
    Writer <$> writerWriteKey <*> maybeToEither' "Failed to derive read capability" writerReader
  where
    writerWriteKey = maybeToEither "Failed to derive write key" . Keys.deriveWriteKey . Keys.toSignatureKey $ keypair
    verificationKeyHash = fmap Digest' . digestFromByteString . Keys.deriveVerificationHash . Keys.toVerificationKey $ keypair
    writerReader = deriveReader <$> writerWriteKey <*> maybeToEither "Failed to interpret verification hash" verificationKeyHash

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a Nothing = Left a
maybeToEither _ (Just b) = Right b

maybeToEither' :: e -> Either e (Maybe a) -> Either e a
maybeToEither' e (Right Nothing) = Left e
maybeToEither' _ (Right (Just r)) = Right r
maybeToEither' _ (Left e) = Left e
