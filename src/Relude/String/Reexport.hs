{-# LANGUAGE CPP                  #-}

#if __GLASGOW_HASKELL__ >= 902
{-# OPTIONS_GHC -Wno-operator-whitespace #-}
#endif

{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE Safe                 #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module                  : Relude.String.Reexport
Copyright               : (c) 2016 Stephen Diehl
                          (c) 2016-2018 Serokell
                          (c) 2018-2023 Kowainik
SPDX-License-Identifier : MIT
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Reexports functions to work with 'Data.Text.Text', 'ByteString' and
'ShortByteString' types.
-}

module Relude.String.Reexport
    ( -- * String
      module Data.String
    , module Text.Read

      -- * Text
    , Text
    , lines
    , unlines
    , words
    , unwords
    , module Data.Text.Encoding
    , module Data.Text.Encoding.Error

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
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8', decodeUtf8With)
import Data.Text.Encoding.Error (OnDecodeError, OnError, UnicodeException, lenientDecode,
                                 strictDecode)
#if __GLASGOW_HASKELL__ >= 904
import Data.Type.Equality (type (~))
#endif
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)
import Text.Read (Read, readMaybe, reads)

import Relude.Base (Constraint, Type)

import qualified Data.Text as Text


-- $setup
-- >>> import Relude

-- | For tracking usage of Text instead of String
type IsText
    (t :: Type)      -- Textual type, e.g. Text, String
    (fun :: Symbol)  -- Function name
    = (t ~ Text, CheckText t fun)

type family CheckText (t :: Type) (fun :: Symbol) :: Constraint where
    CheckText Text _ = ()
    CheckText String fun = TypeError
        ( 'Text "'" ':<>: 'Text fun ':<>: 'Text "' works with 'Text', not 'String'."
        ':$$: 'Text "Possible fixes:"
        ':$$: 'Text "    1. Make sure OverloadedStrings extension is enabled."
        ':$$: 'Text "    2. Apply 'toText' to a single value."
        ':$$: 'Text "    3. Apply 'map toText' to the list value."
        )
    CheckText a fun = TypeError
        ( 'Text "'" ':<>: 'Text fun ':<>: 'Text "' works with 'Text'"
        ':$$: 'Text "But given: '" ':<>: 'ShowType a ':<>: 'Text "'"
        )

{- | 'lines' takes 'Data.Text.Text' and splits it into the list by lines.

Actual type of this function is the following:

@
lines :: 'Data.Text.Text' -> ['Data.Text.Text']
@

but it was given a more complex type to provide friendlier compile time errors.

>>> lines ""
[]
>>> lines "one line"
["one line"]
>>> lines "line 1\nline 2"
["line 1","line 2"]
>>> lines ("string line" :: String)
...
... 'lines' works with 'Text', not 'String'.
      Possible fixes:
          1. Make sure OverloadedStrings extension is enabled.
          2. Apply 'toText' to a single value.
          3. Apply 'map toText' to the list value.
...
>>> lines True
...
... 'lines' works with 'Text'
      But given: 'Bool'
...
-}
lines :: IsText t "lines" => t -> [t]
lines = Text.lines
{-# INLINE lines #-}

{- | 'unlines' takes list of 'Data.Text.Text' values and joins them with line separator.

Actual type of this function is the following:

@
unlines :: ['Data.Text.Text'] -> 'Data.Text.Text'
@

but it was given a more complex type to provide friendlier compile time errors.

>>> unlines []
""
>>> unlines ["line 1"]
"line 1\n"
>>> unlines ["first line", "second line"]
"first line\nsecond line\n"
>>> unlines (["line 1", "line 2"] :: [String])
...
... 'unlines' works with 'Text', not 'String'.
      Possible fixes:
          1. Make sure OverloadedStrings extension is enabled.
          2. Apply 'toText' to a single value.
          3. Apply 'map toText' to the list value.
...
>>> unlines [True, False]
...
... 'unlines' works with 'Text'
      But given: 'Bool'
...
-}
unlines :: IsText t "unlines" => [t] -> t
unlines = Text.unlines
{-# INLINE unlines #-}

{- | 'words' takes 'Data.Text.Text' and splits it into the list by words.

Actual type of this function is the following:

@
words :: 'Data.Text.Text' -> ['Data.Text.Text']
@

but it was given a more complex type to provide friendlier compile time errors.

>>> words ""
[]
>>> words "one line"
["one","line"]
>>> words "   >_<   "
[">_<"]
>>> words ("string words" :: String)
...
... 'words' works with 'Text', not 'String'.
      Possible fixes:
          1. Make sure OverloadedStrings extension is enabled.
          2. Apply 'toText' to a single value.
          3. Apply 'map toText' to the list value.
...
>>> words True
...
... 'words' works with 'Text'
      But given: 'Bool'
...
-}
words :: IsText t "words" => t -> [t]
words = Text.words
{-# INLINE words #-}

{- | 'unwords' takes list of 'Data.Text.Text' values and joins them with space character.

Actual type of this function is the following:

@
unwords :: ['Data.Text.Text'] -> 'Data.Text.Text'
@

but it was given a more complex type to provide friendlier compile time errors.

>>> unwords []
""
>>> unwords ["singleWord"]
"singleWord"
>>> unwords ["word", "another"]
"word another"
>>> unwords (["word", "another"] :: [String])
...
... 'unwords' works with 'Text', not 'String'.
      Possible fixes:
          1. Make sure OverloadedStrings extension is enabled.
          2. Apply 'toText' to a single value.
          3. Apply 'map toText' to the list value.
...
>>> unwords [True, False]
...
... 'unwords' works with 'Text'
      But given: 'Bool'
...
-}
unwords :: IsText t "unwords" => [t] -> t
unwords = Text.unwords
{-# INLINE unwords #-}
