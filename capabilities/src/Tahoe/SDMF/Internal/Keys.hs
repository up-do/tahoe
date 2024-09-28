{- | Key types, derivations, and related functionality for SDMF.

 See docs/specifications/mutable.rst for details.
-}
module Tahoe.SDMF.Internal.Keys where

import Prelude hiding (Read)

import Control.Monad (when)
import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (BlockCipher (ctrCombine), Cipher (cipherInit, cipherKeySize), IV, KeySizeSpecifier (KeySizeFixed), nullIV)
import Crypto.Error (CryptoFailable (CryptoPassed), maybeCryptoError)
import qualified Crypto.PubKey.RSA as RSA
import Crypto.Random (MonadRandom)
import Data.ASN1.BinaryEncoding (DER (DER))
import Data.ASN1.Encoding (ASN1Encoding (encodeASN1), decodeASN1')
import Data.ASN1.Types (ASN1 (End, IntVal, Null, OID, OctetString, Start), ASN1ConstructionType (Sequence), ASN1Object (fromASN1, toASN1))
import Data.Bifunctor (Bifunctor (first))
import Data.Binary (Binary (get, put))
import Data.Binary.Get (getByteString)
import Data.Binary.Put (putByteString)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as B
import Data.ByteString.Base32 (encodeBase32Unpadded)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Data.X509 (PrivKey (PrivKeyRSA), PubKey (PubKeyRSA))
import Tahoe.CHK.Crypto (taggedHash, taggedPairHash)

newtype KeyPair = KeyPair {toPrivateKey :: RSA.PrivateKey} deriving newtype (Show)

toPublicKey :: KeyPair -> RSA.PublicKey
toPublicKey = RSA.private_pub . toPrivateKey

toSignatureKey :: KeyPair -> Signature
toSignatureKey = Signature . toPrivateKey

toVerificationKey :: KeyPair -> Verification
toVerificationKey = Verification . toPublicKey

newtype Verification = Verification {unVerification :: RSA.PublicKey}
    deriving newtype (Eq, Show)

newtype Signature = Signature {unSignature :: RSA.PrivateKey}
    deriving newtype (Eq, Show)

data Write = Write {unWrite :: AES128, writeKeyBytes :: ByteArray.ScrubbedBytes}

instance Eq Write where
    (Write _ left) == (Write _ right) = left == right

instance Binary Write where
    put = putByteString . ByteArray.convert . writeKeyBytes
    get = do
        writeKeyBytes <- ByteArray.convert <$> getByteString keyLength
        let (CryptoPassed unWrite) = cipherInit writeKeyBytes
        pure Write{..}

instance Show Write where
    show (Write _ bs) =
        T.unpack $
            T.concat
                [ "<WriteKey "
                , shorten 4 . showBase32 . ByteArray.convert $ bs
                , ">"
                ]

data Read = Read {unRead :: AES128, readKeyBytes :: ByteArray.ScrubbedBytes}

instance Eq Read where
    (Read _ left) == (Read _ right) = left == right

instance Show Read where
    show (Read _ bs) =
        T.unpack $
            T.concat
                [ "<ReadKey "
                , shorten 4 . showBase32 . ByteArray.convert $ bs
                , ">"
                ]

instance Binary Read where
    put = putByteString . ByteArray.convert . readKeyBytes
    get = do
        readKeyBytes <- ByteArray.convert <$> getByteString keyLength
        let (CryptoPassed unRead) = cipherInit readKeyBytes
        pure Read{..}

newtype StorageIndex = StorageIndex {unStorageIndex :: B.ByteString} deriving newtype (Eq, Ord)

instance Show StorageIndex where
    show (StorageIndex si) =
        T.unpack $
            T.concat
                [ "<SI "
                , shorten 4 . showBase32 . ByteArray.convert $ si
                , ">"
                ]

newtype WriteEnablerMaster = WriteEnablerMaster ByteArray.ScrubbedBytes

newtype WriteEnabler = WriteEnabler ByteArray.ScrubbedBytes

data Data = Data {unData :: AES128, dataKeyBytes :: ByteArray.ScrubbedBytes}

instance Show Data where
    show (Data _ bs) =
        T.unpack $
            T.concat
                [ "<DataKey "
                , shorten 4 . showBase32 . ByteArray.convert $ bs
                , ">"
                ]

instance Eq Data where
    (Data _ left) == (Data _ right) = left == right

instance Binary Data where
    put = putByteString . ByteArray.convert . dataKeyBytes
    get = do
        dataKeyBytes <- ByteArray.convert <$> getByteString keyLength
        let (CryptoPassed unData) = cipherInit dataKeyBytes
        pure Data{..}

newtype SDMF_IV = SDMF_IV (IV AES128)
    deriving (Eq)
    deriving newtype (ByteArray.ByteArrayAccess)

instance Show SDMF_IV where
    show (SDMF_IV iv) = T.unpack . showBase32 . ByteArray.convert $ iv

-- | The size of the public/private key pair to generate.
keyPairBits :: Int
keyPairBits = 2048

-- | The number of bytes in the block cipher key.
keyLength :: Int
(KeySizeFixed keyLength) = cipherKeySize (undefined :: AES128)

{- | Create a new, random key pair (public/private aka verification/signature)
 of the appropriate type and size for SDMF encryption.
-}
newKeyPair :: MonadRandom m => m KeyPair
newKeyPair = do
    (_, priv) <- RSA.generate keyPairBits e
    pure $ KeyPair priv
  where
    e = 0x10001

-- | Compute the write key for a given signature key for an SDMF share.
deriveWriteKey :: Signature -> Maybe Write
deriveWriteKey s =
    Write <$> key <*> pure (ByteArray.convert sbs)
  where
    sbs = taggedHash keyLength mutableWriteKeyTag . signatureKeyToBytes $ s
    key = maybeCryptoError . cipherInit $ sbs

mutableWriteKeyTag :: B.ByteString
mutableWriteKeyTag = "allmydata_mutable_privkey_to_writekey_v1"

-- | Compute the read key for a given write key for an SDMF share.
deriveReadKey :: Write -> Maybe Read
deriveReadKey w =
    Read <$> key <*> pure (ByteArray.convert sbs)
  where
    sbs = taggedHash keyLength mutableReadKeyTag . ByteArray.convert . writeKeyBytes $ w
    key = maybeCryptoError . cipherInit $ sbs

mutableReadKeyTag :: B.ByteString
mutableReadKeyTag = "allmydata_mutable_writekey_to_readkey_v1"

-- | Compute the data encryption/decryption key for a given read key for an SDMF share.
deriveDataKey :: SDMF_IV -> Read -> Maybe Data
deriveDataKey (SDMF_IV iv) r =
    Data <$> key <*> pure (ByteArray.convert sbs)
  where
    -- XXX taggedPairHash has a bug where it doesn't ever truncate so we
    -- truncate for it.
    sbs = B.take keyLength . taggedPairHash keyLength mutableDataKeyTag (B.pack . ByteArray.unpack $ iv) . ByteArray.convert . readKeyBytes $ r
    key = maybeCryptoError . cipherInit $ sbs

mutableDataKeyTag :: B.ByteString
mutableDataKeyTag = "allmydata_mutable_readkey_to_datakey_v1"

-- | Compute the storage index for a given read key for an SDMF share.
deriveStorageIndex :: Read -> StorageIndex
deriveStorageIndex r = StorageIndex si
  where
    si = taggedHash keyLength mutableStorageIndexTag . ByteArray.convert . readKeyBytes $ r

mutableStorageIndexTag :: B.ByteString
mutableStorageIndexTag = "allmydata_mutable_readkey_to_storage_index_v1"

{- | Derive the "write enabler master" secret for a given write key for an
 SDMF share.
-}
deriveWriteEnablerMaster :: Write -> WriteEnablerMaster
deriveWriteEnablerMaster w = WriteEnablerMaster bs
  where
    -- This one shouldn't be truncated.  Set the length to the size of sha256d
    -- output.
    bs = ByteArray.convert . taggedHash 32 mutableWriteEnablerMasterTag . ByteArray.convert . writeKeyBytes $ w

mutableWriteEnablerMasterTag :: B.ByteString
mutableWriteEnablerMasterTag = "allmydata_mutable_writekey_to_write_enabler_master_v1"

{- | Derive the "write enabler" secret for a given peer and "write enabler
 master" for an SDMF share.
-}
deriveWriteEnabler :: B.ByteString -> WriteEnablerMaster -> WriteEnabler
deriveWriteEnabler peerid (WriteEnablerMaster master) = WriteEnabler bs
  where
    -- This one shouldn't be truncated.  Set the length to the size of sha256d
    -- output.
    bs = ByteArray.convert . taggedPairHash 32 mutableWriteEnablerTag (ByteArray.convert master) $ peerid

mutableWriteEnablerTag :: B.ByteString
mutableWriteEnablerTag = "allmydata_mutable_write_enabler_master_and_nodeid_to_write_enabler_v1"

{- | Compute the verification key hash of the given verification key for
 inclusion in an SDMF share.
-}
deriveVerificationHash :: Verification -> B.ByteString
deriveVerificationHash = taggedHash 32 mutableVerificationKeyHashTag . verificationKeyToBytes

{- | The tag used when hashing the verification key to the verification key
 hash for inclusion in SDMF shares.
-}
mutableVerificationKeyHashTag :: B.ByteString
mutableVerificationKeyHashTag = "allmydata_mutable_pubkey_to_fingerprint_v1"

{- | Encode a public key to the Tahoe-LAFS canonical bytes representation -
 X.509 SubjectPublicKeyInfo of the ASN.1 DER serialization of an RSA
 PublicKey.
-}
verificationKeyToBytes :: Verification -> B.ByteString
verificationKeyToBytes = LB.toStrict . encodeASN1 DER . flip toASN1 [] . PubKeyRSA . unVerification

{- | Encode a private key to the Tahoe-LAFS canonical bytes representation -
 X.509 SubjectPublicKeyInfo of the ASN.1 DER serialization of an RSA
 PublicKey.
-}
signatureKeyToBytes :: Signature -> B.ByteString
signatureKeyToBytes = LB.toStrict . encodeASN1 DER . toPKCS8
  where
    -- The ASN1Object instance for PrivKeyRSA can interpret an x509
    -- "Private-Key Information" (aka PKCS8; see RFC 5208, section 5)
    -- structure but it _produces_ some other format.  We must have exactly
    -- this format.
    --
    -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    --
    -- RFC 5208 says:
    --
    --    privateKey is an octet string whose contents are the value of the
    --    private key.  The interpretation of the contents is defined in the
    --    registration of the private-key algorithm.  For an RSA private key,
    --    for example, the contents are a BER encoding of a value of type
    --    RSAPrivateKey.
    --
    -- The ASN.1 BER encoding for a given structure is *not guaranteed to be
    -- unique*.  This means that in general there is no guarantee of a unique
    -- bytes representation of a signature key in this scheme so *key
    -- derivations are not unique*.  If any two implementations disagree on
    -- this encoding (which BER allows them to do) they will not interoperate.
    toPKCS8 (Signature privKey) =
        [ Start Sequence
        , IntVal 0
        , Start Sequence
        , OID [1, 2, 840, 113549, 1, 1, 1]
        , Null
        , End Sequence
        , -- Our ASN.1 encoder doesn't even pretend to support BER.  Use DER!
          -- It results in the same bytes as Tahoe-LAFS is working with so ...
          -- Maybe we're lucky or maybe Tahoe-LAFS isn't actually following
          -- the spec.
          OctetString (LB.toStrict . encodeASN1 DER . toASN1 (PrivKeyRSA privKey) $ [])
        , End Sequence
        ]

-- | Decode a private key from the Tahoe-LAFS canonical bytes representation.
signatureKeyFromBytes :: B.ByteString -> Either String Signature
signatureKeyFromBytes bs = do
    asn1s <- first show $ decodeASN1' DER bs
    (key, extra) <- fromASN1 asn1s
    when (extra /= []) (Left $ "left over data: " <> show extra)
    case key of
        (PrivKeyRSA privKey) -> Right $ Signature privKey
        _ -> Left ("Expect RSA private key, found " <> show key)

-- | Encrypt the signature key for inclusion in the SDMF share itself.
encryptSignatureKey :: Write -> Signature -> B.ByteString
encryptSignatureKey Write{unWrite} = ctrCombine unWrite nullIV . signatureKeyToBytes

{- | Replace most of the tail of a string with a short placeholder.  If the
 string is not much longer than `n` then the result might not actually be
 shorter.

 TODO: Deduplicate this between here and tahoe-chk.
-}
shorten :: Int -> T.Text -> T.Text
shorten n = (<> "...") . T.take n

showBase32 :: B.ByteString -> T.Text
showBase32 = T.toLower . encodeBase32Unpadded
