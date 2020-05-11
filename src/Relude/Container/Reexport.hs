{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2020 Kowainik
SPDX-License-Identifier: MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Reexports all container-related stuff from @base@, @containers@ and
@unordered-containers@ packages.
-}

module Relude.Container.Reexport
    ( module Data.Hashable
    , module Data.HashMap.Strict
    , module Data.HashSet
    , module Data.IntMap.Strict
    , module Data.IntSet
    , module Data.Map.Strict
    , module Data.Sequence
    , module Data.Set
    , module Data.Tuple
    , module GHC.Exts
    ) where

import Data.Hashable (Hashable (hashWithSalt))
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Tuple (curry, fst, snd, swap, uncurry)
import GHC.Exts (IsList (fromList, fromListN))
