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
       ) where

import Relude

{- | Fmaps functions for nested bifunctor. Short for @fmap (bimap f g)@.

>>> bimapF not length $ Just (False, ['a', 'b'])
Just (True,2)
-}
bimapF  :: (Functor f, Bifunctor p) => (a -> c) -> (b -> d) -> f (p a b) -> f (p c d)
bimapF f g = fmap (bimap f g)

{- | Short for @fmap . first@.

>>> firstF not $ Just (False, ['a', 'b'])
Just (True,"ab")
-}
firstF  :: (Functor f, Bifunctor p) => (a -> c) -> f (p a b) -> f (p c b)
firstF = fmap . first

{- | Short for @fmap . second@.

>>> secondF length  $ Just (False, ['a', 'b'])
Just (False,2)
-}
secondF  :: (Functor f, Bifunctor p) => (b -> d) -> f (p a b) -> f (p a d)
secondF = fmap . second
