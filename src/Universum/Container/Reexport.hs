-- | This module reexports all container related stuff from 'Prelude'.

module Universum.Container.Reexport
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
