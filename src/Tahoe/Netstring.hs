{-# LANGUAGE OverloadedStrings #-}

module Tahoe.Netstring (
    netstring,
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8

-- | Encode a bytestring as a netstring.
netstring :: B.ByteString -> B.ByteString
netstring xs =
    B.concat
        [ C8.pack . show . B.length $ xs
        , ":"
        , xs
        , ","
        ]
