{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe              #-}

module List
       ( head
       , ordNub
       , sortOn
       , list
       , product
       , sum
       , unzip
       , unzip3
       , zip
       , zip3
       ) where

import           Control.Monad (return)
import           Data.Foldable (Foldable, foldl', foldr)
import           Data.Function ((.))
import           Data.Functor  (fmap)
import           Data.List     (sortBy, unzip, unzip3, zip, zip3)
import           Data.Maybe    (Maybe (..))
import           Data.Ord      (Ord, comparing)
import qualified Data.Set      as Set
import           GHC.Num       (Num, (*), (+))

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
