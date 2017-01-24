{-# LANGUAGE CPP  #-}
{-# LANGUAGE Safe #-}

module List
       ( list
       , ordNub
       , sortOn
       , unzip
       , unzip3
#if ( __GLASGOW_HASKELL__ >= 800 )
       , whenNotNull
       , whenNotNullM
#endif
       , zip
       , zip3
       ) where

import           Control.Applicative (Applicative)
import           Control.Monad       (Monad (..))
import           Data.Function       ((.))
import           Data.Functor        (fmap)
import           Data.List           (sortBy, unzip, unzip3, zip, zip3)
import           Data.Ord            (Ord, comparing)
import qualified Data.Set            as Set

#if ( __GLASGOW_HASKELL__ >= 800 )
import           Data.List.NonEmpty  as X (NonEmpty (..))
#endif

import           Applicative         (pass)

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

#if ( __GLASGOW_HASKELL__ >= 800 )
whenNotNull :: Applicative f => [a] -> (NonEmpty a -> f ()) -> f ()
whenNotNull []     _ = pass
whenNotNull (x:xs) f = f (x :| xs)
{-# INLINE whenNotNull #-}

whenNotNullM :: Monad m => m [a] -> (NonEmpty a -> m ()) -> m ()
whenNotNullM ml f = ml >>= \l -> whenNotNull l f
{-# INLINE whenNotNullM #-}
#endif
