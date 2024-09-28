module Tahoe.Directory.Internal.Types where

import qualified Data.ByteString as B
import qualified Data.Text as T

-- | A collection of references to other objects.
newtype Directory = Directory
    { directoryChildren :: [Entry]
    }
    deriving (Eq, Show)

-- | A reference to an object of any kind.
data Entry = Entry
    { -- | The name of this entry in its containing directory.  XXX What if UTF-8 decoding fails?
      entryName :: T.Text
    , -- | A capability for reading the contents of this entry. XXX Structured cap instead
      entryReader :: B.ByteString
    , -- | An encrypted capability for performing writes to this entry. XXX
      -- Document the encryption scheme.
      entryEncryptedWriter :: B.ByteString
    , -- | Additional metadata about this entry such as last modification time. XXX How to represent this mixed type collection?
      entryMetadata :: B.ByteString
    }
    deriving (Eq, Show)
