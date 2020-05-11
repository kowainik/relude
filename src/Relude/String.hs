{-# LANGUAGE Safe #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2020 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

Type classes for conversion between different string representations.
-}

module Relude.String
    ( module Relude.String.Reexport
      -- $reexport
    , module Relude.String.Conversion
      -- $conversion
    ) where

import Relude.String.Conversion
import Relude.String.Reexport

{- $reexport
Reexport data types and functions to work with 'Text', 'ByteString',
'ShortByteString'.
-}
{- $conversion
Conversion functions between 'Text', 'String', 'ByteString'.
-}
