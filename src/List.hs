{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module List (
  head,
  ordNub,
  sortOn,
  list,
) where

import Data.List (sortBy)
import Data.Maybe (Maybe(..))
import Data.Ord (Ord, comparing)
import Data.Foldable (Foldable, foldr)
import Data.Function ((.))
import Control.Monad (return)
import qualified Data.Set as Set

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
