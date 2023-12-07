{-# LANGUAGE CPP  #-}
{-# LANGUAGE Safe #-}

{- |
Module                  : Relude.Nub
Copyright               : (c) 2016 Stephen Diehl
                          (c) 2016-2018 Serokell
                          (c) 2018-2023 Kowainik
SPDX-License-Identifier : MIT
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

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

 * 'intNub' is faster when you work with lists of 'Int's.

 * 'intNubOn' is fast with the lists of type that can have fixed number representations.

 * 'sortNub' has better performance than 'ordNub' but should be used when sorting is also needed.

 * 'unstableNub' has better performance than 'hashNub' but doesn't save the original order.
-}

module Relude.Nub
    ( hashNub
    , ordNub

#if MIN_VERSION_containers(0,6,0)
    , ordNubOn
    , intNub
    , intNubOn
#endif

    , sortNub
    , unstableNub
    ) where

#if !MIN_VERSION_hashable(1,4,0)
import Data.Eq (Eq)
#endif
import Data.Hashable (Hashable)
import Data.HashSet as HashSet
import Data.Ord (Ord)
import Prelude (Int, (.))

import qualified Data.Set as Set
#if MIN_VERSION_containers(0,6,0)
import qualified Data.Containers.ListUtils as Containers
#endif


-- $setup
-- >>> import Prelude (div, fromEnum)

{- | Removes duplicate elements from a list, keeping only the first occurrence of
the element.

Like 'Prelude.nub' but runs in \( O(n \log n) \)  time and requires 'Ord'.

>>> ordNub [3, 3, 3, 2, 2, -1, 1]
[3,2,-1,1]

-}
ordNub :: forall a . (Ord a) => [a] -> [a]
#if MIN_VERSION_containers(0,6,0)
ordNub = Containers.nubOrd
{-# INLINE ordNub #-}
#else
ordNub = go Set.empty
  where
    go :: Set.Set a -> [a] -> [a]
    go _ []     = []
    go s (x:xs) =
      if x `Set.member` s
      then go s xs
      else x : go (Set.insert x s) xs
{-# INLINEABLE ordNub #-}
#endif


#if MIN_VERSION_containers(0,6,0)
{- | Similar to 'ordNub' but performs nub through the mapped list on the given
function.

>>> ordNubOn (`div` 10) [3, 3, 3, 13, 2, 22, -1, 1, 66]
[3,13,22,-1,66]

@since 1.0.0.0
-}
ordNubOn :: forall b a . (Ord b) => (a -> b) -> [a] -> [a]
ordNubOn = Containers.nubOrdOn
{-# INLINE ordNubOn #-}
#endif

{- | Like 'Prelude.nub' but runs in \( O(n \log_{16} n) \)  time and requires 'Hashable'.

>>> hashNub [3, 3, 3, 2, 2, -1, 1]
[3,2,-1,1]

-}
#if MIN_VERSION_hashable(1,4,0)
hashNub :: forall a . (Hashable a) => [a] -> [a]
#else
hashNub :: forall a . (Eq a, Hashable a) => [a] -> [a]
#endif
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
#if MIN_VERSION_hashable(1,4,0)
unstableNub :: (Hashable a) => [a] -> [a]
#else
unstableNub :: (Eq a, Hashable a) => [a] -> [a]
#endif
unstableNub = HashSet.toList . HashSet.fromList
{-# INLINE unstableNub #-}


#if MIN_VERSION_containers(0,6,0)
{- | Removes duplicate elements from a list, keeping only the first occurance of
the element.

Like 'Prelude.nub' but runs in \( O (n \min (n, int\_bits )) \)  time and requires 'Ord'.

>>> intNub [3, 3, 3, 2, 2, -1, 1]
[3,2,-1,1]

@since 1.0.0.0
-}
intNub :: [Int] -> [Int]
intNub = Containers.nubInt

{-# INLINE intNub #-}

{- | Similar to 'intNub' but works on lists of any types by performing "nubbing" through 'Int's.

>>> intNubOn fromEnum "ababbbcdaffee"
"abcdfe"

@since 1.0.0.0
-}
intNubOn :: (a -> Int) -> [a] -> [a]
intNubOn = Containers.nubIntOn
{-# INLINE intNubOn #-}

#endif
