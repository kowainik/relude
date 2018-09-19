{- |
Copyright: (c) 2016 Stephen Diehl
           (c) 20016-2018 Serokell
           (c) 2018 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Functions to remove duplicates from a list.

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

module Relude.Nub
       ( hashNub
       , ordNub
       , sortNub
       , unstableNub
       , foldNub
       , hashFoldNub
       , slowFoldNub
       , foldNub'
       , hashFoldNub'
       , slowFoldNub'
       ) where

import Data.Eq (Eq)
import Data.Foldable (Foldable, foldr, foldl', elem)
import Data.Hashable (Hashable)
import Data.Ord (Ord)
import Prelude ((.), flip)

import qualified Data.HashSet as HashSet
import qualified Data.Set as Set

{- | Like 'Prelude.nub' but runs in @O(n * log n)@ time and requires 'Ord'.

>>> ordNub [3, 3, 3, 2, 2, -1, 1]
[3,2,-1,1]

-}
ordNub :: (Ord a) => [a] -> [a]
ordNub = go Set.empty
  where
    go _ []     = []
    go s (x:xs) =
      if x `Set.member` s
      then go s xs
      else x : go (Set.insert x s) xs

{- | Like 'Prelude.nub' but runs in @O(n * log_16(n))@ time and requires 'Hashable'.

>>> hashNub [3, 3, 3, 2, 2, -1, 1]
[3,2,-1,1]

-}
hashNub :: (Eq a, Hashable a) => [a] -> [a]
hashNub = go HashSet.empty
  where
    go _ []     = []
    go s (x:xs) =
      if x `HashSet.member` s
      then go s xs
      else x : go (HashSet.insert x s) xs

{- | Like 'ordNub' but also sorts a list.

>>> sortNub [3, 3, 3, 2, 2, -1, 1]
[-1,1,2,3]

-}
sortNub :: (Ord a) => [a] -> [a]
sortNub = Set.toList . Set.fromList

{- | Like 'hashNub' but has better performance and also doesn't save the order.

>>> unstableNub [3, 3, 3, 2, 2, -1, 1]
[1,2,3,-1]

-}
unstableNub :: (Eq a, Hashable a) => [a] -> [a]
unstableNub = HashSet.toList . HashSet.fromList

{- | A generalized nub for any 'Foldable'. Not necessarily order preserving. -}
foldNub :: (Foldable t, Ord a) => t a -> Set.Set a
foldNub = foldr Set.insert Set.empty

{- | A strict variant of 'foldNub' using 'foldl'' instead of 'foldr'-}
foldNub' :: (Foldable t, Ord a) => t a -> Set.Set a
foldNub' = foldl' (flip Set.insert) Set.empty

{- | A generalized nub for any 'Foldable' containing 'Hashable' -}
hashFoldNub :: (Foldable t, Eq a, Hashable a) => t a -> HashSet.HashSet a
hashFoldNub = foldr HashSet.insert HashSet.empty

{- | A strict variant of 'hashFoldNub' using 'foldl'' instead of 'foldr'-}
hashFoldNub' :: (Foldable t, Eq a, Hashable a) => t a -> HashSet.HashSet a
hashFoldNub' = foldl' (flip HashSet.insert) HashSet.empty

{- | A slow @O(n^2)@ nub, with minimal constraints -}
slowFoldNub :: (Foldable t, Eq a) => t a -> [a]
slowFoldNub = foldr (\a xs->  if a `elem` xs then xs else a:xs) []

{- | A strict variant of 'slowFoldNub' using 'foldl'' instead of 'foldr' -}
slowFoldNub' :: (Foldable t, Eq a) => t a -> [a]
slowFoldNub' = foldl' (\xs a->  if a `elem` xs then xs else a:xs) []
