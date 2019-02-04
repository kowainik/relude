{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Reexports functions to work with 'Text' and 'ByteString' types.
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
       , module Data.ByteString
       ) where

import Data.ByteString (ByteString)
import Data.String (IsString (..))
import Data.Text (Text, lines, unlines, unwords, words)
import Data.Text.Encoding (decodeUtf8', decodeUtf8With)
import Data.Text.Encoding.Error (OnDecodeError, OnError, UnicodeException, lenientDecode,
                                 strictDecode)
import Text.Read (Read, readMaybe, reads)
