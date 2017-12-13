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
       , module Data.Vector
       ) where

import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Vector (Vector)
