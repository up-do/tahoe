{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Tahoe.CHK.Capability (
    CHK (..),
    Reader,
    readKey,
    verifier,
    Verifier,
    storageIndex,
    fingerprint,
    required,
    total,
    size,
    makeReader,
    pCapability,
    pVerifier,
    pReader,
    dangerRealShow,
) where

import Control.Lens (view)
import Control.Lens.TH (makeLenses)
import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (Cipher (cipherInit))
import Crypto.Error (maybeCryptoError)
import Data.ByteArray (convert)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base32 as B
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.TreeDiff.Class (ToExpr (..))
import Data.Void (Void)
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import Tahoe.CHK.Cipher (Key (..))
import Tahoe.CHK.Crypto (storageIndexHash)
import qualified Tahoe.CHK.Parsing
import Text.Megaparsec (ErrorFancy (ErrorFail), Parsec, count, fancyFailure, oneOf, try, (<|>))
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (decimal)

{- | Define a type in which we will perform parsing.  There is no custom error
 data (Void) and we are parsing T.Text.
-}
type Parser = Parsec Void T.Text

{- | The maximum number of shares it is possible for CHK-encoded data to be
 divided in to.
-}
maxShares :: Word16
maxShares = 256

-- | The maximum size of the application data represented by a set of shares.
maxDataSize :: Integer
maxDataSize = fromIntegral (maxBound :: Word64)

{- | Represent a CHK "verify" capability.  This capability type can be used to
 verify the existence and validity (bit-level) of shares for the associated
 piece of plaintext.

 It can also be used to repair unhealthy data (I think?)
-}
data Verifier = Verifier
    { -- | The storage index of a verify capability is used as the key into the
      -- content-addressable storage system that is a storage server.  It can be
      -- used to ask storage servers for "shares" (ciphertext plus some
      -- metadata) to download.
      _storageIndex :: B.ByteString
    , -- | The fingerprint (aka "UEB hash" aka "URI extension block hash") is a
      -- cryptographic hash that covers the URI extension block at the end of a
      -- CHK share.  The URI extension block itself contains various other
      -- cryptographic hashes.  Altogether this allows for integrity checking so
      -- shares downloaded from storage servers can be checked for validity (ie,
      -- that they are the same as what was uploaded) before they are processed.
      _fingerprint :: B.ByteString
    , -- | The number of shares required to ZFEC decode the contents of the
      -- shares.  ZFEC calls this *K*.  It must be that 1 <= required <= 256 and
      -- required <= total.  ZFEC is not defined outside of these bounds.
      _required :: Word16
    , -- | The total number of shares produced by ZFEC encoding.  ZFEC calls
      -- this *n*.  It must be that 1 <= total <= 256 and required <= total.
      _total :: Word16
    , -- | The size (in bytes) of the plaintext encoded in the shares.  It must
      -- be that size >= 0 and in practice it is usually true that size >= 56.
      _size :: Integer
    }
    deriving (Ord, Eq, Generic, ToExpr)

$(makeLenses ''Verifier)

{- | Replace most of the tail of a string with a short placeholder.  If the
 string is not much longer than `n` then the result might not actually be
 shorter.
-}
shorten :: Int -> T.Text -> T.Text
shorten n = (<> "...") . T.take n

-- | Show a value as Text.
showT :: Show s => s -> T.Text
showT = T.pack . show

-- | Show a ByteString using a base32-encoded representation.
showBase32 :: B.ByteString -> T.Text
showBase32 = T.toLower . B.encodeBase32Unpadded

-- | A version of bounded specialized to parsing text.
bounded :: (Ord n, Integral n) => n -> n -> Parser n
bounded = Tahoe.CHK.Parsing.bounded decimal

instance Show Verifier where
    show v =
        T.unpack $
            T.intercalate
                ":"
                [ "URI"
                , "CHK-Verifier"
                , shorten 4 . showBase32 $ view storageIndex v
                , shorten 4 . showBase32 $ view fingerprint v
                , showT $ view required v
                , showT $ view total v
                , showT $ view size v
                ]

{- | Represent a CHK "read" capability.  This capability type can be diminished
 to a verify capability so it confers all of the abilities of a verify
 capability.  It can also be used to decrypt shares to reconstruct the
 original plaintext.  See makeReader for a safe constructor that correctly
 derives the verify capability.
-}
data Reader = Reader
    { -- | The read key of a read capability is used as the symmetric encryption
      -- key to turn the original plaintext into ciphertext and back again.  The
      -- read key is also used to derive the verify key for the verify
      -- capability.  See ``storageIndexHash``.
      _readKey :: Key AES128
    , -- | The verify capability for this read capability.
      _verifier :: Verifier
    }

$(makeLenses ''Reader)

-- AESKey128 has no Eq or Ord instances so derive these for Reader manually.
-- We do include the AESKey128 in our comparison by encoding it to bytes
-- first.
instance Eq Reader where
    left == right = readerKey left == readerKey right

instance Ord Reader where
    compare left right = compare (readerKey left) (readerKey right)

instance ToExpr Reader where
    toExpr = toExpr . readerKey

{- | Give it a Show instance that elides the sensitive material.  This makes
 it easier to compose with other types and we can still learn a lot of
 useful things about a capability without being able to see the literal
 secret key.
-}
instance Show Reader where
    show reader =
        T.unpack $
            T.intercalate
                ":"
                [ "URI"
                , "CHK"
                , shorten 4 . showBase32 . convert . keyBytes $ view readKey reader
                , shorten 4 . showBase32 $ view (verifier . fingerprint) reader
                , showT $ view (verifier . required) reader
                , showT $ view (verifier . total) reader
                , showT $ view (verifier . size) reader
                ]

-- Construct a key with Eq and Ord instances for the Reader Eq and Ord
-- instances.
readerKey :: Reader -> (B.ByteString, Verifier)
readerKey r = (convert $ view readKey r, view verifier r)

{- | A "Content-Hash-Key" (CHK) capability is small value that can be used to
 perform some operation on a (usually) larger value that may be stored
 somewhere else.  There are two forms of CHK capabilities: verify and read.
 See *Verifier* and *Reader* for details.
-}
data CHK = CHKVerifier Verifier | CHKReader Reader deriving (Ord, Eq)

{- | Serialize a CHK capability to text.  This operation is "dangerous" in
 that it will serialize the encryption key of a read capability into the
 text.  Since the encryption key is necessary and (practically) sufficient
 to recover the original plaintext associated with the capability, it must
 be handled carefully to avoid unintentional disclosure.  Serializing the
 key to a string is a good way to accidentally disclose it!  Be warned.

 The text is in the canonical form, originally used by the Python
 implementation of Tahoe-LAFS.
-}
dangerRealShow :: CHK -> T.Text
dangerRealShow (CHKVerifier v) =
    T.intercalate
        ":"
        [ "URI"
        , "CHK-Verifier"
        , showBase32 $ view storageIndex v
        , showBase32 $ view fingerprint v
        , showT $ view required v
        , showT $ view total v
        , showT $ view size v
        ]
dangerRealShow (CHKReader r) =
    T.intercalate
        ":"
        [ "URI"
        , "CHK"
        , showBase32 . convert $ view readKey r
        , showBase32 $ view (verifier . fingerprint) r
        , showT $ view (verifier . required) r
        , showT $ view (verifier . total) r
        , showT $ view (verifier . size) r
        ]

{- | A parser combinator for parsing either a verify or read CHK capability
 from the canonical format.  This is the moral inverse of dangerRealShow.
-}
pCapability :: Parser CHK
pCapability = try (CHKVerifier <$> pVerifier) <|> (CHKReader <$> pReader)

-- | A parser combinator for parsing a CHK verify capability.
pVerifier :: Parser Verifier
pVerifier =
    Verifier
        <$> (string "URI:CHK-Verifier:" *> pBase32 rfc3548Alphabet 128)
        <* char ':'
        <*> pBase32 rfc3548Alphabet 256
        <* char ':'
        <*> bounded 1 maxShares
        <* char ':'
        <*> bounded 1 maxShares
        <* char ':'
        <*> bounded 1 maxDataSize

-- | A parser combinator for parsing a CHK read capability.
pReader :: Parser Reader
pReader =
    makeReader
        <$> ( string "URI:CHK:"
                *> pBase32 rfc3548Alphabet 128
                >>= maybe (fancyFailure . Set.singleton . ErrorFail . T.unpack $ "Failed to build AESKey128 from CHK read key bytes") pure . maybeCryptoError . cipherInit
            )
        <* char ':'
        <*> pBase32 rfc3548Alphabet 256
        <* char ':'
        <*> bounded 1 256
        <* char ':'
        <*> bounded 1 256
        <* char ':'
        <*> bounded 1 maxDataSize

{- | Construct a CHK read capability from its components.  This includes the
 correct derivation of the corresponding CHK verify capability.
-}
makeReader :: Key AES128 -> B.ByteString -> Word16 -> Word16 -> Integer -> Reader
makeReader readKey' fingerprint' required' total' size' =
    Reader readKey' (deriveVerifier readKey' fingerprint' required' total' size')

{- | Given all of the fields of a CHK read capability, derive and return the
 corresponding CHK verify capability.
-}
deriveVerifier ::
    -- | The read key
    Key AES128 ->
    -- | The fingerprint
    B.ByteString ->
    -- | The required number of shares
    Word16 ->
    -- | The total number of shares
    Word16 ->
    -- | The plaintext size
    Integer ->
    Verifier
deriveVerifier = Verifier . storageIndexHash

{- | A parser combinator for an arbitrary byte string of a fixed length,
 encoded using base32.
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
