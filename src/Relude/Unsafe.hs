{-# LANGUAGE Unsafe #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2021 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

__⚠️ Warning ⚠️__

This module contains unsafe partial functions. They are unavoidable
sometimes, but we encourage you to use safer analogues:

+-----------------------------------+---------------------------------------------------------------------+
| __Partial__                       | __Total__                                                           |
+===================================+=====================================================================+
| @'head' :: [a] -> a@              | @'Relude.head' :: NonEmpty a -> a@                                  |
+-----------------------------------+---------------------------------------------------------------------+
| @'tail' :: [a] -> [a]@            | @'Relude.tail' :: NonEmpty a -> [a]@                                |
+-----------------------------------+---------------------------------------------------------------------+
| @'read' :: Read a => String -> a@ | @'Relude.String.Reexport.readMaybe' :: Read a => String -> Maybe a@ |
+-----------------------------------+---------------------------------------------------------------------+
| @'fromJust' :: Maybe a -> a@      | @'Relude.Monad.Reexport.fromMaybe' :: a -> Maybe a -> a@            |
+-----------------------------------+---------------------------------------------------------------------+

This module is intended to be imported qualified and it is not
included in default prelude exports.

@
__import qualified__ Relude.Unsafe as Unsafe

foo :: [a] -> a
foo = Unsafe.'head'
@
-}

module Relude.Unsafe
    ( -- * Unsafe list functions
      head
    , tail
    , last
    , init
    , (!!)
    , at
      -- * Unsafe 'Data.Maybe.Maybe' functions
    , fromJust
      -- * Unsafe "Text.Read" functions
    , read
    ) where

import Data.List (head, init, last, tail, (!!))
import Data.Maybe (fromJust)
import Text.Read (read)

import Relude.Function (flip)
import Relude.Numeric (Int)


-- $setup
-- >>> import Relude

{- | Similar to '!!' but with flipped arguments.
get element from list using index value starting from `0`.

>>> at 2 ["a", "b", "c"]
"c"

it is also useful when used in a partially applied position like:

>>> map (at 1) [["a","b","c"], ["a","b","c"], ["a","b","c"]]
["b","b","b"]
-}
at :: Int -> [a] -> a
at = flip (!!)
