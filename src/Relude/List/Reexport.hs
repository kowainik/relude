{-# LANGUAGE Trustworthy #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2021 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

Reexports most of the "Data.List".
-}

module Relude.List.Reexport
    ( -- * List
      module Data.List
    , cycle
    , sortWith
    ) where

import Data.List (break, drop, dropWhile, filter, genericDrop, genericLength, genericReplicate,
                  genericSplitAt, genericTake, group, inits, intercalate, intersperse, isPrefixOf,
                  iterate, map, permutations, repeat, replicate, reverse, scanl, scanl', scanl1,
                  scanr, scanr1, sort, sortBy, sortOn, span, splitAt, subsequences, tails, take,
                  takeWhile, transpose, uncons, unfoldr, unzip, unzip3, zip, zip3, zipWith, (++))
import GHC.Exts (sortWith)


-- $setup
-- >>> import Relude

{- | Creates an infinite list from a finite list by appending the list
to itself infinite times (i.e. by cycling the list). Unlike @cycle@
from "Data.List", this implementation doesn't throw error on empty
lists, but returns an empty list instead.

>>> cycle []
[]
>>> take 10 $ cycle [1,2,3]
[1,2,3,1,2,3,1,2,3,1]
-}
cycle :: [a] -> [a]
cycle [] = []
cycle xs = cycledList
  where
    cycledList = xs ++ cycledList
