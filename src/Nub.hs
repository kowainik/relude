-- | Functions to remove dublicates from a list.

module Nub
       ( hashNub
       , ordNub
       , sortNub
       , unstableNub
       ) where

import           Data.Eq       (Eq)
import           Data.Hashable (Hashable)
import           Data.HashSet  as HashSet
import           Data.Ord      (Ord)
import qualified Data.Set      as Set
import           Prelude       ((.))

-- | Like 'Prelude.nub' but runs in @O(n * log n)@ time and requires 'Ord'.
--
-- >>> ordNub [3, 3, 3, 2, 2, 1]
-- [3, 2, 1]
ordNub :: (Ord a) => [a] -> [a]
ordNub = go Set.empty
  where
    go _ []     = []
    go s (x:xs) =
      if x `Set.member` s
      then go s xs
      else x : go (Set.insert x s) xs

-- | Like 'Prelude.nub' but runs in @O(n * log_16(n))@ time and requires 'Hashable'.
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
-- >>> sortNub [3, 3, 3, 2, 2, 1]
-- [1, 2, 3]
sortNub :: (Ord a) => [a] -> [a]
sortNub = Set.toList . Set.fromList

-- | Like 'hashNub' but has better performance and also doesn't save the order.
unstableNub :: (Eq a, Hashable a) => [a] -> [a]
unstableNub = HashSet.toList . HashSet.fromList

-- * Performance
{-|
 Here are recomendations of usage of particular functions based on benchmarking resutls.
 Benchmarks were made on lists of 'Prelude.Int's and 'Data.Text.Text's.

 * 'hashNub' is faster than 'ordNub' when there're not so many different values in the list.

 * 'hashNub' is the fastest with 'Data.Text.Text'.

 * 'sortNub' has better performance than 'ordNub' but should be used when sorting is also needed.

 * 'unstableNub' has better performance than 'hashNub'.

-}
