-- | Parsing and serialization for directories and their entries.
module Tahoe.Directory.Internal.Parsing where

import Control.Monad (void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Void (Void)
import Tahoe.Directory.Internal.Types (Directory (..), Entry (..))
import Text.Megaparsec (MonadParsec (eof, label, takeP), ParseErrorBundle, Parsec, many, parse)
import Text.Megaparsec.Byte (string)
import Text.Megaparsec.Byte.Lexer (decimal)

-- | Parse the serialized form of a directory into a Directory.
parse :: B.ByteString -> Either (ParseErrorBundle B.ByteString Void) Directory
parse = Text.Megaparsec.parse pDirectory "Directory"

-- | The parser type we will parse in.
type Parser = Parsec Void B.ByteString

-- XXX This doesn't do bounds checking.

-- | Parse the base ten representation of a natural number.
natural :: Integral i => Parser i
natural = decimal

{- | Parse a netstring-encoded value, applying a sub-parser to the encoded
 string.
-}
pNetstring ::
    -- | A function that takes the length of the string encoded in the
    -- netstring and returns a parser for the value the encoded string
    -- represents.
    (Int -> Parser a) ->
    -- | A parser for the value.
    Parser a
pNetstring pInner = do
    len <- natural
    void $ string ":"
    result <- pInner len
    void $ string ","
    pure result

pDirectory :: Parser Directory
pDirectory = Directory <$> (many pEntry <* eof)

pEntry :: Parser Entry
pEntry =
    label "entry" $
        pNetstring $ \_ ->
            Entry
                <$> label "name" (pNetstring pUTF8)
                <*> label "ro_uri" (pNetstring (takeP Nothing))
                <*> label "rw_uri" (pNetstring (takeP Nothing))
                <*> label "metadata" (pNetstring (takeP Nothing))

pUTF8 :: Int -> Parser T.Text
pUTF8 n = do
    bs <- takeP Nothing n
    either (\e -> fail $ "UTF-8 parsing failed: " <> show e) pure (decodeUtf8' bs)

-- | Serialize a Directory to the canonical bytes representation.
serialize :: Directory -> B.ByteString
serialize Directory{directoryChildren} = B.concat $ serializeEntry <$> directoryChildren

serializeEntry :: Entry -> B.ByteString
serializeEntry Entry{..} =
    -- XXX The name must be NFC normalized apparently, try unicode-transforms
    -- library.  Perhaps we should enforce normalization in the Entry
    -- constructor?
    netstring . B.concat $
        [ netstring . encodeUtf8 $ entryName
        , netstring entryReader
        , netstring entryEncryptedWriter
        , netstring entryMetadata
        ]

-- | Encode a bytestring as a netstring.
netstring :: B.ByteString -> B.ByteString
netstring xs =
    B.concat
        [ C8.pack . show . B.length $ xs
        , ":"
        , xs
        , ","
        ]
