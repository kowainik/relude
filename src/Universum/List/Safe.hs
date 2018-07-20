{-# LANGUAGE Safe #-}

{-
Copyright: (c) 2016 Stephen Diehl
           (c) 20016-2018 Serokell
           (c) 2018 Kowainik
License: MIT
-}

-- | This module contains safe functions to work with list type (mostly with 'NonEmpty').

module Universum.List.Safe
       ( list
       , uncons
       , whenNotNull
       , whenNotNullM
       ) where

import Universum.Applicative (Applicative, pass)
import Universum.Functor (fmap)
import Universum.List.Reexport (NonEmpty (..))
import Universum.Monad (Monad (..))
import Universum.Monad (Maybe (..))

-- $setup
-- >>> import Universum.Applicative (pure)
-- >>> import Universum.Base ((==), even)
-- >>> import Universum.Bool (Bool (..), not)
-- >>> import Universum.Foldable (length)
-- >>> import Universum.Function (($))
-- >>> import Universum.Print (print)

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

-- | Destructuring list into its head and tail if possible. This function is total.
--
-- >>> uncons []
-- Nothing
-- >>> uncons [1..5]
-- Just (1,[2,3,4,5])
-- >>> uncons (5 : [1..5]) >>= \(f, l) -> pure $ f == length l
-- Just True
uncons :: [a] -> Maybe (a, [a])
uncons []     = Nothing
uncons (x:xs) = Just (x, xs)

{- | Performs given action over 'NonEmpty' list if given list is non empty.

>>> whenNotNull [] $ \(b :| _) -> print (not b)
>>> whenNotNull [False,True] $ \(b :| _) -> print (not b)
True

-}
whenNotNull :: Applicative f => [a] -> (NonEmpty a -> f ()) -> f ()
whenNotNull []     _ = pass
whenNotNull (x:xs) f = f (x :| xs)
{-# INLINE whenNotNull #-}

-- | Monadic version of 'whenNotNull'.
whenNotNullM :: Monad m => m [a] -> (NonEmpty a -> m ()) -> m ()
whenNotNullM ml f = ml >>= \l -> whenNotNull l f
{-# INLINE whenNotNullM #-}
