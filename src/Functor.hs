{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe              #-}

module Functor
       ( Functor (..)
       , void
       , ($>)
       , (<$>)
       , (<<$>>)
       ) where

import           Data.Function ((.))
import           Data.Functor  (Functor (..), void, ($>), (<$>))

infixl 4 <<$>>
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
