{-# LANGUAGE CPP         #-}
{-# LANGUAGE Trustworthy #-}

-- | Utility functions to work with lists.

module List
       ( list
       , hashNub
       , ordNub
       , sortBy
       , sortOn
       , sortWith
       , unzip
       , unzip3
#if ( __GLASGOW_HASKELL__ >= 800 )
       , whenNotNull
       , whenNotNullM
#endif
       , zip
       , zip3
       ) where

import           Data.Eq             (Eq)
import           Data.Functor        (fmap)
import           Data.Hashable       (Hashable)
import           Data.HashSet        as HS
import           Data.List           (sortBy, sortOn, unzip, unzip3, zip, zip3)
import           Data.Ord            (Ord)
import qualified Data.Set            as Set
import           GHC.Exts            (sortWith)

#if ( __GLASGOW_HASKELL__ >= 800 )
import           Control.Applicative (Applicative)
import           Control.Monad       (Monad (..))
import           Data.List.NonEmpty  as X (NonEmpty (..))

import           Applicative         (pass)
#endif

-- | Like 'Prelude.nub' but runs in @O(n * log n)@ time and requires 'Ord'.
ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ []     = []
    go s (x:xs) =
      if x `Set.member` s
      then go s xs
      else x : go (Set.insert x s) xs

-- | Like 'Prelude.nub' but runs in @O(n * log_16(n))@ time and requires 'Hashable'.
hashNub :: (Eq a, Hashable a) => [a] -> [a]
hashNub l = go HS.empty l
  where
    go _ []     = []
    go s (x:xs) =
      if x `HS.member` s
      then go s xs
      else x : go (HS.insert x s) xs

-- | Returns default list if given list is empty.
-- Otherwise applies given function to every element.
--
-- >>> list [True] even []
-- [True]
-- >>> list [True] even [1..5]
-- [False,True,False,True,False]
list :: [b] -> (a -> b) -> [a] -> [b]
list def f xs = case xs of
    [] -> def
    _  -> fmap f xs

#if ( __GLASGOW_HASKELL__ >= 800 )
-- | Performs given action over 'NonEmpty' list if given list is non empty.
whenNotNull :: Applicative f => [a] -> (NonEmpty a -> f ()) -> f ()
whenNotNull []     _ = pass
whenNotNull (x:xs) f = f (x :| xs)
{-# INLINE whenNotNull #-}

-- | Monadic version of 'whenNotNull'.
whenNotNullM :: Monad m => m [a] -> (NonEmpty a -> m ()) -> m ()
whenNotNullM ml f = ml >>= \l -> whenNotNull l f
{-# INLINE whenNotNullM #-}
#endif
