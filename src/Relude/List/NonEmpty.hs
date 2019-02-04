{-# LANGUAGE Safe #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module contains safe functions to work with list type in terms of 'NonEmpty'.
-}

module Relude.List.NonEmpty
       ( viaNonEmpty
       , whenNotNull
       , whenNotNullM
       ) where

import Relude.Applicative (Applicative, pass)
import Relude.Function ((.))
import Relude.Functor (fmap)
import Relude.List.Reexport (NonEmpty (..), nonEmpty)
import Relude.Monad (Maybe (..), Monad (..))

-- $setup
-- >>> import Relude

{- | For safe work with lists using functinons for 'NonEmpty'.

>>> viaNonEmpty head [1]
Just 1
>>> viaNonEmpty head []
Nothing
-}
viaNonEmpty :: (NonEmpty a -> b) -> [a] -> Maybe b
viaNonEmpty f = fmap f . nonEmpty
{-# INLINE viaNonEmpty #-}

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
