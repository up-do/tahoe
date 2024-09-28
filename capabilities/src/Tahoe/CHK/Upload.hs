{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Tahoe.CHK.Upload (
    UploadResult (uploadResultReadCap, uploadResultExistingShares, uploadResultShareMap),
    Uploadable (..),
    Parameters (Parameters),
    defaultParameters,
    filesystemUploadable,
    filesystemUploadableWithConvergence,
    filesystemUploadableRandomConvergence,
    memoryUploadableWithConvergence,
    getConvergentKey,
    upload,
    store,
    prettyFormatSharemap,
    adjustSegmentSize,
    encryptAndEncode,
) where

import Control.Monad.Conc.Class (
    modifyIORefCAS,
 )

import Data.Maybe (
    fromJust,
 )

import Data.List (
    intersperse,
 )

import Data.IORef (
    newIORef,
 )

import qualified Data.Binary as Binary
import Data.ByteArray (ScrubbedBytes)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.Text (
    Text,
    intercalate,
    pack,
 )
import qualified Data.Text as Text

import qualified Data.Set as Set

import qualified Data.Map.Strict as Map

import qualified Tahoe.CHK.Capability as Cap

import System.IO (
    IOMode (ReadMode),
    hFileSize,
    hSetBinaryMode,
    openBinaryFile,
    openFile,
 )

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (BlockCipher, Cipher (cipherInit, cipherKeySize), KeySizeSpecifier (..))
import "cryptonite" Crypto.Random (getRandomBytes)

import Tahoe.CHK.Cipher (Key)
import Tahoe.CHK.Crypto (
    convergenceEncryptionHashLazy,
    storageIndexHash,
 )

import Tahoe.CHK.Server (
    ShareMap,
    StorageServer (..),
 )
import Tahoe.CHK.Types (
    Parameters (Parameters),
    ShareNum,
    Size,
    StorageIndex,
 )

import Tahoe.Util (nextMultipleOf)

import Crypto.Error (maybeCryptoError)
import Data.Tuple.Extra (thd3)
import Tahoe.CHK (
    encode,
 )
import Tahoe.CHK.Encrypt (encryptLazy)

-- Some data that can be uploaded.
data Uploadable = Uploadable
    { uploadableKey :: Key AES128
    , uploadableSize :: Size
    , uploadableParameters :: Parameters
    , uploadableReadCleartext :: Integer -> IO B.ByteString
    }

-- The outcome of an attempt to upload an immutable.
data UploadResult = UploadResult
    { uploadResultReadCap :: Cap.Reader
    , uploadResultExistingShares :: Integer
    , uploadResultShareMap :: ShareMap
    }
    deriving (Show)

-- Find shares that already exist on servers.
locateAllShareholders :: StorageIndex -> [StorageServer] -> IO ShareMap
locateAllShareholders storageIndex servers =
    Map.unionsWith Set.union <$> mapM getBuckets servers
  where
    getBuckets :: StorageServer -> IO ShareMap
    getBuckets s = do
        buckets <- storageServerGetBuckets s storageIndex
        return $ Map.fromSet (const $ Set.singleton s) buckets

planSharePlacement :: Parameters -> StorageIndex -> ShareMap -> [StorageServer] -> ShareMap
planSharePlacement (Parameters _ total _ _) _storageIndex _currentShares servers =
    Map.fromList
        [ (shareNum, Set.singleton server)
        | (shareNum, server) <- zip [0 .. (fromIntegral total - 1)] (cycle servers)
        ]

-- Upload some immutable share data to some buckets on some servers.
--
-- XXX TODO This writes the raw share data to a file with no server-side
-- bookkeeping.  This may not be intrinsically bad but it makes the share
-- files incompatible with the Tahoe-LAFS storage server on-disk state.  This
-- may not be intrinsically bad either but interop testing would be much
-- easier if the on-disk state were compatible.
uploadImmutableShares ::
    StorageIndex ->
    [(ShareNum, StorageServer, BL.ByteString)] ->
    IO ()
uploadImmutableShares storageIndex uploads =
    uploadChunks 0
  where
    -- How much data to upload to each server per request.
    chunkSize = 1024 * 1024

    -- Upload the chunk of each share at `offset` to the corresponding server
    -- and then proceed to the chunks at the next offset.
    uploadChunks offset = do
        res <- uploadChunk chunkSize storageIndex offset
        if res
            then uploadChunks (offset + chunkSize)
            else pure ()

    uploadChunk ::
        Integer ->
        StorageIndex ->
        Integer ->
        IO Bool
    uploadChunk size storageIndex' offset =
        if any ("" /=) chunks
            then mapM_ (uploadOneChunk offset storageIndex') uploads >> pure True
            else pure False
      where
        chunks = map (BL.take (fromIntegral size) . BL.drop (fromIntegral offset) . thd3) uploads

    uploadOneChunk offset storageIndex' (shareNum, server, shareData) =
        storageServerWrite server storageIndex' shareNum offset (BL.toStrict shareData)

{- | Encrypt and encode some application data to some ZFEC shares and upload
 them to some servers.
-}
store ::
    -- | The servers to consider using.
    [StorageServer] ->
    -- | The application data to operate on.
    Uploadable ->
    -- | The result of the attempt.
    IO UploadResult
store servers uploadable@(Uploadable key _ params _) =
    encryptAndEncode uploadable >>= upload servers key params

{- | Given some cleartext and some encoding parameters: encrypt and encode some
 shares that can later be used to reconstruct the cleartext.
-}
encryptAndEncode ::
    -- | The application data to encrypt and encode.
    Uploadable ->
    -- | An action to get an action that can be repeatedly evaluated to get
    -- share data.  As long as there is more share data, it evaluates to Left.
    -- When shares are done, it evaluates to Right.
    IO ([BL.ByteString], Cap.Reader)
encryptAndEncode (Uploadable readKey _ params read') = do
    plaintext <- readAll read'
    let ciphertext = encryptLazy readKey plaintext
    (shares, cap) <- encode readKey params ciphertext
    pure (map Binary.encode shares, cap)
  where
    readAll :: (Integer -> IO B.ByteString) -> IO BL.ByteString
    readAll f = do
        bs <- BL.fromStrict <$> f (1024 * 32)
        if bs == ""
            then pure ""
            else (bs <>) <$> readAll f

{- | Given some cleartext, some encoding parameters, and some servers:
 encrypt, encode, and upload some shares that can later be used to
 reconstruct the cleartext.

 This replaces allmydata.immutable.upload.Uploader.upload.
-}
upload ::
    -- | The servers to consider uploading shares to.
    [StorageServer] ->
    -- | The encryption key (to derive the storage index).
    Key AES128 ->
    -- | The encoding parameters (XXX only for happy, right?)
    Parameters ->
    -- | The share data to upload.
    ([BL.ByteString], Cap.Reader) ->
    -- | Describe the outcome of the upload.
    IO UploadResult
upload servers key params encoded = do
    -- Decide where to put it
    existingShares <- locateAllShareholders storageIndex servers
    let targets = targetServers params existingShares

    -- Go
    let (streams, cap) = encoded
    uploadImmutableShares storageIndex (uploads targets streams)

    return $
        UploadResult
            { uploadResultReadCap = cap
            , uploadResultExistingShares = fromIntegral $ length existingShares
            , uploadResultShareMap = targets
            }
  where
    storageIndex :: StorageIndex
    storageIndex = storageIndexHash key

    targetServers :: Parameters -> ShareMap -> ShareMap
    targetServers parameters existingShares = planSharePlacement parameters storageIndex existingShares servers

    -- Adapt a stream of share data to a stream of share data annotated with
    -- server placement decision.
    uploads ::
        ShareMap ->
        [BL.ByteString] ->
        [(ShareNum, StorageServer, BL.ByteString)]
    uploads goal shareDatav = Map.foldrWithKey (makeUpload shareDatav) [] goal
      where
        makeUpload ::
            [BL.ByteString] ->
            ShareNum ->
            Set.Set StorageServer ->
            [(ShareNum, StorageServer, BL.ByteString)] ->
            [(ShareNum, StorageServer, BL.ByteString)]
        makeUpload shareDatav' num servers' soFar =
            [ ( num
              , server
              , shareDatav' !! fromIntegral num
              )
            | server <- Set.elems servers'
            ]
                ++ soFar

defaultParameters :: Parameters
defaultParameters = Parameters (128 * 1024) 10 7 3

-- The adjustment implemented in
-- allmydata.immutable.upload.BaseUploadable.get_all_encoding_parameters
adjustSegmentSize :: Parameters -> Size -> Parameters
adjustSegmentSize (Parameters segmentSize total happy required) dataSize =
    Parameters (effectiveSegmentSize dataSize) total happy required
  where
    effectiveSegmentSize =
        -- For small files, shrink the segment size to avoid wasting space.
        -- Also make the shrunk value a multiple of required shares or the
        -- encoding doesn't work.
        nextMultipleOf required . min segmentSize

-- Create an uploadable with the given key.
filesystemUploadable :: Key AES128 -> FilePath -> Parameters -> IO Uploadable
filesystemUploadable key path params = do
    fhandle <- openBinaryFile path ReadMode
    fsize <- hFileSize fhandle
    return $
        Uploadable
            { uploadableKey = key
            , uploadableSize = fsize
            , uploadableParameters = adjustSegmentSize params fsize
            , -- TODO Consider replacing this with a lazy bytestring or a list of bytestrings
              uploadableReadCleartext = B.hGet fhandle . fromIntegral
            }

filesystemUploadableWithConvergence :: B.ByteString -> FilePath -> Parameters -> IO Uploadable
filesystemUploadableWithConvergence secret uploadablePath params = do
    -- Allow getConvergentKey to use lazy ByteString to read and hash the
    -- uploadable by letting the handle remain open past the end of this
    -- function.  lazy ByteString will close the handle.
    uploadableHandle <- openFile uploadablePath ReadMode
    hSetBinaryMode uploadableHandle True

    -- Annoyingly, adjust the segment size here so that the convergence secret
    -- is computed based on the adjusted value.  This is what Tahoe-LAFS does so
    -- it is necessary to arrive at the same converged key.  Whether this part
    -- of the construction is actually important aside from interop, I don't
    -- know.
    size <- hFileSize uploadableHandle
    content <- BL.hGetContents uploadableHandle

    memoryUploadableWithConvergence secret size content params

-- TODO Consider lazy bytestring here instead
memoryUploadableWithConvergence :: B.ByteString -> Integer -> BL.ByteString -> Parameters -> IO Uploadable
memoryUploadableWithConvergence secret size content params =
    let key = getConvergentKey secret (adjustSegmentSize params size) content
     in memoryUploadable key size content params

memoryUploadable :: Key AES128 -> Integer -> BL.ByteString -> Parameters -> IO Uploadable
memoryUploadable key size content params =
    let makeReader :: BL.ByteString -> IO (Integer -> IO BL.ByteString)
        makeReader allContent =
            let cas len content' = (BL.drop len content', BL.take len content')
             in do
                    contentRef <- newIORef allContent
                    return $ (modifyIORefCAS contentRef . cas) . fromIntegral
     in do
            reader <- makeReader content
            return $
                Uploadable
                    { uploadableKey = key
                    , uploadableSize = size
                    , uploadableParameters = adjustSegmentSize params size
                    , -- TODO Consider replacing this with a lazy bytestring or a list of bytestrings
                      uploadableReadCleartext = (BL.toStrict <$>) . reader
                    }

-- allmydata.immutable.upload.FileHandle._get_encryption_key_convergent
getConvergentKey :: B.ByteString -> Parameters -> BL.ByteString -> Key AES128
getConvergentKey secret params content =
    fromJust . maybeCryptoError . cipherInit $ convergenceEncryptionHashLazy secret params content

buildKeyIO :: forall cipher. BlockCipher cipher => IO (Key cipher)
buildKeyIO = do
    keyBytes <- getRandomBytes @IO @ScrubbedBytes keySize
    pure . fromJust . maybeCryptoError . cipherInit $ keyBytes
  where
    keySize = case cipherKeySize @cipher undefined of
        KeySizeRange _ high -> high
        KeySizeEnum [] -> error "no key sizes!"
        KeySizeEnum (s : _) -> s
        KeySizeFixed s -> s

-- Create an uploadable with a random key.
filesystemUploadableRandomConvergence :: FilePath -> Parameters -> IO Uploadable
filesystemUploadableRandomConvergence path params = do
    key <- buildKeyIO :: IO (Key AES128)
    filesystemUploadable key path params

prettyFormatSharemap :: ShareMap -> Text
prettyFormatSharemap sharemap =
    intercalate
        "\n"
        [ Text.concat ["\t", showElem elem']
        | elem' <- Map.toList sharemap
        ]
  where
    showElem :: (ShareNum, Set.Set StorageServer) -> Text
    showElem (shareNum, servers) =
        Text.concat $
            [ pack $ show shareNum
            , ":"
            ]
                ++ intersperse ", " (map storageServerID (Set.toList servers))
