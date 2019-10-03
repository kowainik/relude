{- |
Copyright:  (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Useful combinators for bifunctors inside functors. This set of functions is
useful when you want to work with types like these ones:

@
foo :: IO (Either a b)
bar :: IO (a, b)

baz :: Maybe (Either a b)
qux :: Maybe (a, b)
@
-}

module Relude.Extra.Bifunctor
       ( bimapF
       , firstF
       , secondF
       , bimapBoth
       ) where

import Relude


{- | Fmaps functions for nested bifunctor. Short for @fmap (bimap f g)@.

>>> bimapF not length $ Just (False, ['a', 'b'])
Just (True,2)
-}
bimapF  :: (Functor f, Bifunctor p) => (a -> c) -> (b -> d) -> f (p a b) -> f (p c d)
bimapF f g = fmap (bimap f g)
{-# INLINE bimapF #-}

{- | Short for @fmap . first@.

>>> firstF not $ Just (False, ['a', 'b'])
Just (True,"ab")
-}
firstF  :: (Functor f, Bifunctor p) => (a -> c) -> f (p a b) -> f (p c b)
firstF = fmap . first
{-# INLINE firstF #-}

{- | Short for @fmap . second@.

>>> secondF length  $ Just (False, ['a', 'b'])
Just (False,2)
-}
secondF  :: (Functor f, Bifunctor p) => (b -> d) -> f (p a b) -> f (p a d)
secondF = fmap . second
{-# INLINE secondF #-}

{- | Maps a function over both elements of a bifunctor.

>>> bimapBoth length ("a", "bb")
(1,2)
>>> bimapBoth length ("foo", "a", "bb")
("foo",1,2)
>>> map (bimapBoth not) [Left True, Right False]
[Left False,Right True]
>>> import Data.Functor.Const
>>> getConst $ bimapBoth (+ 1) (Const 41)
42
-}
bimapBoth :: Bifunctor f => (a -> b) -> f a a -> f b b
bimapBoth f = bimap f f
{-# INLINE bimapBoth #-}
