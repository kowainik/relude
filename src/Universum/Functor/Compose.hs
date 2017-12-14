{-# LANGUAGE Safe #-}

module Universum.Functor.Compose
       ( map
       , (<<$>>)
       ) where

import Universum.Function ((.))
import Universum.Functor.Reexport (Functor (..))

-- | 'Prelude.map' generalized to 'Functor'.
map :: Functor f => (a -> b) -> f a -> f b
map = fmap

-- | Alias for @fmap . fmap@. Convenient to work with two nested 'Functor's.
--
-- >>> negate <<$>> Just [1,2,3]
-- Just [-1,-2,-3]
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
infixl 4 <<$>>
