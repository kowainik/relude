{-# LANGUAGE Safe #-}

{-
Copyright: (c) 2016 Stephen Diehl
           (c) 20016-2018 Serokell
           (c) 2018 Kowainik
License: MIT
-}

-- | This module contains safe functions to work with list type (mostly with 'NonEmpty').

module Universum.List.Safe
       ( viaNonEmpty
       , uncons
       , whenNotNull
       , whenNotNullM
       ) where

import Universum.Applicative (Applicative, pass)
import Universum.Function ((.))
import Universum.Functor (fmap)
import Universum.List.Reexport (NonEmpty (..), nonEmpty)
import Universum.Monad (Maybe (..), Monad (..))

-- $setup
-- >>> import Universum.Applicative (pure)
-- >>> import Universum.Base ((==))
-- >>> import Universum.Bool (Bool (..), not)
-- >>> import Universum.Foldable (length)
-- >>> import Universum.Function (($))
-- >>> import Universum.List.Reexport (head)
-- >>> import Universum.Print (print)

{- | For safe work with lists using functinons for 'NonEmpty'.

>>> viaNonEmpty head [1]
Just 1
>>> viaNonEmpty head []
Nothing

-}
viaNonEmpty :: (NonEmpty a -> b) -> [a] -> Maybe b
viaNonEmpty f = fmap f . nonEmpty
{-# INLINE viaNonEmpty #-}

{- | Destructuring list into its head and tail if possible. This function is total.

>>> uncons []
Nothing
>>> uncons [1..5]
Just (1,[2,3,4,5])
>>> uncons (5 : [1..5]) >>= \(f, l) -> pure $ f == length l
Just True

-}
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
