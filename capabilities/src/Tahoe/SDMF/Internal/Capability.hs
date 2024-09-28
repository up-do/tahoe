-- | Structured representations of SDMF capabilities.
module Tahoe.SDMF.Internal.Capability where

import Prelude hiding (Read)

import Control.Applicative ((<|>))
import Control.Monad (void)
import Crypto.Hash (digestFromByteString)
import Data.Binary (decode)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as B
import qualified Data.ByteString.Base32 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Void (Void)
import Data.Word (Word16)
import Tahoe.CHK.SHA256d (Digest' (Digest'), SHA256d)
import Tahoe.Capability (ConfidentialShowable (..))
import Tahoe.SDMF.Internal.Keys (
    Read (readKeyBytes),
    StorageIndex (StorageIndex, unStorageIndex),
    Write (writeKeyBytes),
    deriveReadKey,
    deriveStorageIndex,
    readKeyBytes,
    showBase32,
 )
import Text.Megaparsec (
    ErrorFancy (ErrorFail),
    Parsec,
    count,
    failure,
    fancyFailure,
    oneOf,
 )
import Text.Megaparsec.Char (char, string)

-- | Any kind of SDMF capability.
data SDMF
    = SDMFVerifier Verifier
    | SDMFReader Reader
    | SDMFWriter Writer
    deriving (Eq, Show)

instance ConfidentialShowable SDMF where
    confidentiallyShow = dangerRealShow

-- | A verify capability for an SDMF object.
data Verifier = Verifier
    { verifierStorageIndex :: StorageIndex
    , verifierVerificationKeyHash :: Digest' SHA256d
    }
    deriving (Eq, Show)

instance Ord Verifier where
    a <= b = verifierStorageIndex a <= verifierStorageIndex b

instance ConfidentialShowable Verifier where
    confidentiallyShow = dangerRealShow . SDMFVerifier

-- | A read capability for an SDMF object.
data Reader = Reader
    { readerReadKey :: Read
    , readerVerifier :: Verifier
    }
    deriving (Eq, Show)

instance Ord Reader where
    a <= b = readerVerifier a <= readerVerifier b

instance ConfidentialShowable Reader where
    confidentiallyShow = dangerRealShow . SDMFReader

-- | A write capability for an SDMF object.
data Writer = Writer
    { writerWriteKey :: Write
    , writerReader :: Reader
    }
    deriving (Eq, Show)

instance Ord Writer where
    a <= b = writerReader a <= writerReader b

instance ConfidentialShowable Writer where
    confidentiallyShow = dangerRealShow . SDMFWriter

-- | Diminish a write key to a read key and wrap it in a reader capability.
deriveReader :: Write -> Digest' SHA256d -> Maybe Reader
deriveReader w fingerprint = Reader <$> readKey <*> verifier
  where
    readKey = deriveReadKey w
    verifier = flip deriveVerifier fingerprint <$> readKey

-- | Diminish a read key to a verify key and wrap it in a verifier capability.
deriveVerifier :: Read -> Digest' SHA256d -> Verifier
deriveVerifier readKey = Verifier storageIndex
  where
    storageIndex = deriveStorageIndex readKey

type Parser = Parsec Void T.Text

-- | A parser for any kind of SDMF capability type.
pCapability :: Parser SDMF
pCapability = (SDMFVerifier <$> pVerifier) <|> (SDMFReader <$> pReader) <|> (SDMFWriter <$> pWriter)

-- | A parser for an SDMF verifier capability.
pVerifier :: Parser Verifier
pVerifier = uncurry Verifier <$> pPieces "URI:SSK-Verifier:" StorageIndex

-- | A parser for an SDMF reader capability.
pReader :: Parser Reader
pReader = do
    (readKey, verificationKeyHash) <- pPieces "URI:SSK-RO:" (decode . LB.fromStrict)
    let verifier = deriveVerifier readKey verificationKeyHash
    pure $ Reader readKey verifier

-- | A parser for an SDMF writer capability.
pWriter :: Parser Writer
pWriter = do
    (writeKey, verificationKeyHash) <- pPieces "URI:SSK:" (decode . LB.fromStrict)
    let reader = deriveReader writeKey verificationKeyHash
    case Writer writeKey <$> reader of
        Nothing -> failure Nothing mempty
        Just writer -> pure writer

{- | A parser for two base32-encoded bytestrings with some given prefix,
 formatted as they are in the string representation of an SDMF capability.
-}
pPieces ::
    -- | The prefix to expect.
    T.Text ->
    -- | A function to convert the first bytestring to a result value.
    (B.ByteString -> a) ->
    -- | A parser for the two pieces of the SDMF capability.
    Parser (a, Digest' SHA256d)
pPieces prefix convertSecret = do
    void $ string prefix
    secret <- convertSecret <$> pBase32 rfc3548Alphabet 128
    void $ char ':'
    digestBytes <- pBase32 rfc3548Alphabet 256
    case digestFromByteString digestBytes of
        Nothing -> failure Nothing mempty
        Just verificationKeyHash ->
            pure (secret, Digest' verificationKeyHash)

{- | A parser combinator for an arbitrary byte string of a fixed length,
 encoded using base32.

 TODO: Avoid duplicating this implementation here and in tahoe-chk.
-}
pBase32 ::
    -- | The alphabet to use.  For example, *rfc3548Alphabet*.
    [Char] ->
    -- | The number of bits in the encoded byte string.
    Word16 ->
    -- | A parser for the byte string.  Strings that are not valid base32 will
    -- be rejected.  Strings that are the wrong length are *not necessarily*
    -- currently rejected!  Please fix that, somebody.
    Parser B.ByteString
pBase32 alpha bits = do
    b32Text <- pBase32Text
    either (fancyFailure . Set.singleton . ErrorFail . T.unpack) pure (decodeBase32Text b32Text)
  where
    decodeBase32Text = B.decodeBase32Unpadded . T.encodeUtf8
    pBase32Text = T.snoc <$> stem <*> trailer

    -- Determine how many full characters to expect along with how many bits
    -- are left to expect encoded in the final character.
    (full, extra) = bits `divMod` 5

    -- Match the base32 characters that represent the full 5 bits
    -- possible.  fromIntegral is okay here because `full` is only a
    -- Word16 and will definitely fit safely into the Int count wants.
    stem :: Parser T.Text
    stem = T.pack <$> count (fromIntegral full) (oneOf alpha)

    -- Match the final character that represents fewer than 5 bits.
    trailer :: Parser Char
    trailer = oneOf $ trailingChars alpha extra

    -- XXX The real trailing character set is smaller than this.  This
    -- parser will let through invalid characters that result in giving us
    -- possibly too many bits.
    trailingChars :: [Char] -> Word16 -> [Char]
    trailingChars alpha' _ = alpha'

{- | The RFC3548 standard alphabet used by Gnutella, Content-Addressable Web,
 THEX, Bitzi, Web-Calculus...
-}
rfc3548Alphabet :: [Char]
rfc3548Alphabet = "abcdefghijklmnopqrstuvwxyz234567"

-- | Show an SDMF capability, including all secret information.
{-# DEPRECATED dangerRealShow "Use the ConfidentialShowable instance" #-}
dangerRealShow :: SDMF -> T.Text
dangerRealShow (SDMFVerifier Verifier{verifierStorageIndex, verifierVerificationKeyHash}) =
    T.concat
        [ "URI:SSK-Verifier:"
        , showBase32 . unStorageIndex $ verifierStorageIndex
        , ":"
        , showBase32 . ByteArray.convert $ verifierVerificationKeyHash
        ]
dangerRealShow (SDMFReader Reader{readerReadKey, readerVerifier}) =
    T.concat
        [ "URI:SSK-RO:"
        , showBase32 . ByteArray.convert . readKeyBytes $ readerReadKey
        , ":"
        , showBase32 . ByteArray.convert . verifierVerificationKeyHash $ readerVerifier
        ]
dangerRealShow (SDMFWriter Writer{writerWriteKey, writerReader}) =
    T.concat
        [ "URI:SSK:"
        , showBase32 . ByteArray.convert . writeKeyBytes $ writerWriteKey
        , ":"
        , showBase32 . ByteArray.convert . verifierVerificationKeyHash . readerVerifier $ writerReader
        ]
