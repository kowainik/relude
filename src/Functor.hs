{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Functor (
  Functor(..),
  ($>),
  (<$>),
  void,
) where

#if (__GLASGOW_HASKELL__ >= 710)
import Data.Functor (
    Functor(..)
  , ($>)
  , (<$>)
  , void
  )
#else
import Data.Functor (
    Functor(..)
  , (<$>)
  )

import Data.Function (flip)
import Data.Function ((.))

infixl 4 $>

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

void :: Functor f => f a -> f ()
void x = () <$ x
#endif
