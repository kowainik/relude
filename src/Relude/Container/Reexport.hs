{-# LANGUAGE Trustworthy #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2021 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

Reexports container-related data types, functions and typeclasses from @base@,
@containers@ and @unordered-containers@ packages.
-}

module Relude.Container.Reexport
    ( -- * "Data.Hashable" reexports
      Hashable (..)
      -- * "Data.HashMap.Strict" reexports
    , HashMap
      -- * "Data.HashSet" reexports
    , HashSet
      -- * "Data.IntMap.Strict" reexports
    , IntMap
      -- * "Data.IntSet" reexports
    , IntSet
      -- * "Data.Map.Strict" reexports
    , Map
      -- * "Data.Sequence" reexports
    , Seq
      -- * "Data.Set" reexports
    , Set
      -- * "Data.Tuple" reexports
    , curry
    , fst
    , snd
    , swap
    , uncurry
      -- * "GHC.Exts" reexports
    , IsList (..)
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
