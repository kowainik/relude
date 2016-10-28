{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe              #-}

module Functor
       ( Functor (..)
       , void
       , ($>)
       , (<$>)
       ) where

#if (__GLASGOW_HASKELL__ >= 710)
import           Data.Functor  (Functor (..), void, ($>), (<$>))
#else
import           Data.Function (flip, (.))
import           Data.Functor  (Functor (..), (<$>))

infixl 4 $>

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

-- TODO: define it properly to be able to export
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

void :: Functor f => f a -> f ()
void x = () <$ x
#endif
