{-# LANGUAGE Safe #-}

{-
Copyright: (c) 2016 Stephen Diehl
           (c) 20016-2018 Serokell
           (c) 2018 Kowainik
License: MIT
-}

-- | This module contains safe functions to work with list type (mostly with 'NonEmpty').

module Relude.List.Safe
       ( viaNonEmpty
       , uncons
       , whenNotNull
       , whenNotNullM
       ) where

import Relude.Applicative (Applicative, pass)
import Relude.Function ((.))
import Relude.Functor (fmap)
import Relude.List.Reexport (NonEmpty (..), nonEmpty)
import Relude.Monad (Maybe (..), Monad (..))

-- $setup
-- >>> import Relude.Applicative (pure)
-- >>> import Relude.Base ((==))
-- >>> import Relude.Bool (Bool (..), not)
-- >>> import Relude.Foldable (length)
-- >>> import Relude.Function (($))
-- >>> import Relude.List.Reexport (head)
-- >>> import Relude.Print (print)

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
