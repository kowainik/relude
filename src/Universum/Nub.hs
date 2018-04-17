{-| Functions to remove duplicates from a list.

 = Performance
 To check the performance there was done a bunch of benchmarks.
 Benchmarks were made on lists of 'Prelude.Int's and 'Data.Text.Text's.
 There were two types of list to use:

 * Lists which consist of many different elements

 * Lists which consist of many same elements


 Here are some recomendations for usage of particular functions based on benchmarking resutls.

 * 'hashNub' is faster than 'ordNub' when there're not so many different values in the list.

 * 'hashNub' is the fastest with 'Data.Text.Text'.

 * 'sortNub' has better performance than 'ordNub' but should be used when sorting is also needed.

 * 'unstableNub' has better performance than 'hashNub' but doesn't save the original order.
-}

module Universum.Nub
       ( hashNub
       , ordNub
       , sortNub
       , unstableNub
       ) where

import Data.Eq (Eq)
import Data.Hashable (Hashable)
import Data.HashSet as HashSet
import Data.Ord (Ord)
import Data.Set (Set)
import Prelude (Bool, Char, (.))

import qualified Data.Set as Set

-- Liquid Haskell check for duplicates.
{-@ type ListUnique a = {v : [a] | NoDups v} @-}

{-@ predicate NoDups L = Set_emp (dups L) @-}

{-@ measure dups :: [a] -> (Set a)
    dups ([])   = {v | Set_emp v}
    dups (x:xs) = {v | v =
      if (Set_mem x (listElts xs))
      then (Set_cup (Set_sng x) (dups xs))
      else (dups xs)}
@-}

{-@ Set.toList :: Set a -> ListUnique a @-}

-- | Like 'Prelude.nub' but runs in @O(n * log n)@ time and requires 'Ord'.
--
-- >>> ordNub [3, 3, 3, 2, 2, -1, 1]
-- [3,2,-1,1]
ordNub :: (Ord a) => [a] -> [a]
ordNub = go Set.empty
  where
    go _ []     = []
    go s (x:xs) =
      if x `Set.member` s
      then go s xs
      else x : go (Set.insert x s) xs

-- | Like 'Prelude.nub' but runs in @O(n * log_16(n))@ time and requires 'Hashable'.
--
-- >>> hashNub [3, 3, 3, 2, 2, -1, 1]
-- [3,2,-1,1]
hashNub :: (Eq a, Hashable a) => [a] -> [a]
hashNub = go HashSet.empty
  where
    go _ []     = []
    go s (x:xs) =
      if x `HashSet.member` s
      then go s xs
      else x : go (HashSet.insert x s) xs

-- | Like 'ordNub' but also sorts a list.
--
-- >>> sortNub [3, 3, 3, 2, 2, -1, 1]
-- [-1,1,2,3]
{-@ sortNub :: [a] -> ListUnique a @-}
sortNub :: (Ord a) => [a] -> [a]
sortNub = Set.toList . Set.fromList

-- | Like 'hashNub' but has better performance and also doesn't save the order.
--
-- >>> unstableNub [3, 3, 3, 2, 2, -1, 1]
-- [1,2,3,-1]
unstableNub :: (Eq a, Hashable a) => [a] -> [a]
unstableNub = HashSet.toList . HashSet.fromList
