{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe              #-}

module Bifunctor
       ( Bifunctor (..)
       ) where

import           Control.Applicative (Const (..))
import           Data.Either         (Either (..))
import           Data.Function       (id, (.))

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

instance Bifunctor (,) where
  bimap f g ~(a, b) = (f a, g b)

instance Bifunctor Either where
  bimap f _ (Left a)  = Left (f a)
  bimap _ g (Right b) = Right (g b)

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)
