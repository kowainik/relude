{-# LANGUAGE CPP  #-}
{-# LANGUAGE Safe #-}

-- | Convenient functions to work with 'Functor'.

module Universum.Functor
       ( Functor (..)
       , void
       , ($>)
       , (<$>)
       , (<<$>>)
       ) where

import Data.Function ((.))
import Data.Functor (Functor (..), void, ($>), (<$>))

-- | Alias for @fmap . fmap@. Convenient to work with two nested 'Functor's.
--
-- >>> negate <<$>> Just [1,2,3]
-- Just [-1,-2,-3]
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
infixl 4 <<$>>
