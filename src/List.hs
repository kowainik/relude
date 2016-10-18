{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module List (
  head,
  ordNub,
  sortOn,
  list,
  product,
  sum
) where

import Data.List (sortBy)
import Data.Maybe (Maybe(..))
import Data.Ord (Ord, comparing)
import Data.Foldable (Foldable, foldr, foldl')
import Data.Function ((.))
import Data.Functor (fmap)
import Control.Monad (return)
import qualified Data.Set as Set
import GHC.Num (Num, (+), (*))

head :: (Foldable f) => f a -> Maybe a
head = foldr (\x _ -> return x) Nothing

sortOn :: (Ord o) => (a -> o) -> [a] -> [a]
sortOn = sortBy . comparing

-- O(n * log n)
ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ []     = []
    go s (x:xs) =
      if x `Set.member` s
      then go s xs
      else x : go (Set.insert x s) xs

list :: [b] -> (a -> b) -> [a] -> [b]
list def f xs = case xs of
  [] -> def
  _  -> fmap f xs

{-# INLINE product #-}
product :: (Foldable f, Num a) => f a -> a
product = foldl' (*) 1

{-# INLINE sum #-}
sum :: (Foldable f, Num a) => f a -> a
sum = foldl' (+) 0
