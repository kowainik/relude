{-# LANGUAGE CPP  #-}
{-# LANGUAGE Safe #-}

{- |
Copyright: (c) 2016 Stephen Diehl
           (c) 2016-2018 Serokell
           (c) 2018 Kowainik
SPDX-License-Identifier: MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module contains useful functions to work with 'Functor' type class.
-}

module Relude.Functor.Fmap
       ( (<<$>>)
       , (<&>)
       , flap
       , (??)
       ) where

import Relude.Function ((.))
import Relude.Functor.Reexport (Functor (..))

import Data.Functor ((<$>))
#if MIN_VERSION_base(4,11,0)
import Data.Functor ((<&>))
#else


-- | Flipped version of '<$>'.
infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as
{-# INLINE (<&>) #-}
#endif

-- $setup
-- >>> import Relude.Monad (Maybe (..))
-- >>> import Relude.List ((++))
-- >>> import Relude.Numeric (negate, (*), (+))

{- | Alias for @fmap . fmap@. Convenient to work with two nested 'Functor's.

>>> negate <<$>> Just [1,2,3]
Just [-1,-2,-3]
-}
infixl 4 <<$>>
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
{-# INLINE (<<$>>) #-}

{- | Takes a function in a 'Functor' context and applies it to a normal value.

>>> flap (++) "relude" "P"
"Prelude"
-}
flap :: Functor f => f (a -> b) -> a -> f b
flap ff x = (\f -> f x) <$> ff
{-# INLINE flap #-}

{- | Operator version of the 'flap' function.

>>> [(+2), (*3)] ?? 5
[7,15]

>>> Just (+3) ?? 5
Just 8
-}
infixl 4 ??
(??) :: Functor f => f (a -> b) -> a -> f b
(??) = flap
{-# INLINE (??) #-}
