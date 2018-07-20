module Universum.Extra.Bifunctor
       ( bimapF
       , firstF
       , secondF
       ) where

import Universum

bimapF  :: (Functor f, Bifunctor p) => (a -> c) -> (b -> d) -> f (p a b) -> f (p c d)
bimapF f g = fmap (bimap f g)

firstF  :: (Functor f, Bifunctor p) => (a -> c) -> f (p a b) -> f (p c b)
firstF = fmap . first

secondF  :: (Functor f, Bifunctor p) => (b -> d) -> f (p a b) -> f (p a d)
secondF = fmap . second
