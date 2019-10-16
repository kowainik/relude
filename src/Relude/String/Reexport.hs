{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2019 Kowainik
SPDX-License-Identifier: MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Reexports functions to work with 'Text', 'ByteString'
and 'ShortByteString' types.
-}

module Relude.String.Reexport
       ( -- * String
         module Data.String

         -- * Text
       , module Data.Text
       , module Data.Text.Encoding
       , module Data.Text.Encoding.Error
       , module Text.Read

         -- * ByteString
       , ByteString

         -- * ShortByteString
       , ShortByteString
       , toShort
       , fromShort
       ) where

import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import Data.String (IsString (..), String)
import Data.Text (Text, lines, unlines, unwords, words)
import Data.Text.Encoding (decodeUtf8', decodeUtf8With)
import Data.Text.Encoding.Error (OnDecodeError, OnError, UnicodeException, lenientDecode,
                                 strictDecode)
import Text.Read (Read, readMaybe, reads)
