{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2019 Kowainik
SPDX-License-Identifier: MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Functions to remove duplicates from a list.

 = Performance
 To check the performance there was done a bunch of benchmarks.
 Benchmarks were made on lists of 'Prelude.Int's and 'Data.Text.Text's.
 There were two types of list to use:

 * Lists which consist of many different elements

 * Lists which consist of many same elements


 Here are some recommendations for usage of particular functions based on benchmarking results.

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
       ) where

import Data.Eq (Eq)
import Data.Hashable (Hashable)
import Data.HashSet as HashSet
import Data.Ord (Ord)
import Prelude ((.))

import qualified Data.Set as Set


{- | Like 'Prelude.nub' but runs in \( O(n \log n) \)  time and requires 'Ord'.

>>> ordNub [3, 3, 3, 2, 2, -1, 1]
[3,2,-1,1]

-}
ordNub :: forall a . (Ord a) => [a] -> [a]
ordNub = go Set.empty
  where
    go :: Set.Set a -> [a] -> [a]
    go _ []     = []
    go s (x:xs) =
      if x `Set.member` s
      then go s xs
      else x : go (Set.insert x s) xs
{-# INLINEABLE ordNub #-}

{- | Like 'Prelude.nub' but runs in \( O(n \log_{16} n) \)  time and requires 'Hashable'.

>>> hashNub [3, 3, 3, 2, 2, -1, 1]
[3,2,-1,1]

-}
hashNub :: forall a . (Eq a, Hashable a) => [a] -> [a]
hashNub = go HashSet.empty
  where
    go :: HashSet.HashSet a -> [a] -> [a]
    go _ []     = []
    go s (x:xs) =
      if x `HashSet.member` s
      then go s xs
      else x : go (HashSet.insert x s) xs
{-# INLINEABLE hashNub #-}

{- | Like 'ordNub' runs in \( O(n \log n) \)  but also sorts a list.

>>> sortNub [3, 3, 3, 2, 2, -1, 1]
[-1,1,2,3]

-}
sortNub :: (Ord a) => [a] -> [a]
sortNub = Set.toList . Set.fromList
{-# INLINE sortNub #-}

{- | Like 'hashNub' runs in \( O(n \log_{16} n) \) but has better performance; it doesn't save the order.

>>> unstableNub [3, 3, 3, 2, 2, -1, 1]
[1,2,3,-1]

-}
unstableNub :: (Eq a, Hashable a) => [a] -> [a]
unstableNub = HashSet.toList . HashSet.fromList
{-# INLINE unstableNub #-}
