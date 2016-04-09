{-# LANGUAGE CPP #-}
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

infixl 4 $>

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

void :: Functor f => f a -> f ()
void x = () <$ x
#endif
