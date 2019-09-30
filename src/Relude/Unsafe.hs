{-# LANGUAGE Unsafe #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Unsafe functions to work with lists and 'Maybe'. Sometimes unavoidable but it's
better not to use them. This module is intended to be imported qualified and
it's not even included in default prelude exports.

@
import qualified Relude.Unsafe as Unsafe

foo :: [a] -> a
foo = Unsafe.head
@
-}

module Relude.Unsafe
       ( module Data.List
       , module Data.Maybe
       , at
       ) where

import Data.List (head, init, last, tail, (!!))
import Data.Maybe (fromJust)

import Relude.Function (flip)
import Relude.Numeric (Int)

{- | Similar to '!!' but with flipped arguments.
get element from list using index value starting from `0`.

>>> at 2 ["a","b,"c"]
"c"

it is also usefull when used in a partially applied position like:
>>> map (at 1) [["a","b","c"],["a","b","c"],["a","b","c"]]
["b","b","b"]

-}
at :: Int -> [a] -> a
at = flip (!!)
