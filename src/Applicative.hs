{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Applicative (
  orAlt,
  orEmpty,
  eitherA,
  guarded,
  guardedA,
  purer,
  liftAA2,
  (<<*>>),
) where

import Data.Bool (Bool, bool)
import Data.Function ((.))
import Data.Functor (Functor)
import Data.Either (Either(..))
import Data.Monoid (Monoid(..))
import Control.Applicative

orAlt :: (Alternative f, Monoid a) => f a -> f a
orAlt f = f <|> pure mempty

orEmpty :: Alternative f => Bool -> a -> f a
orEmpty b a = if b then pure a else empty

eitherA :: (Alternative f) => f a -> f b -> f (Either a b)
eitherA a b = (Left <$> a) <|> (Right <$> b)

guarded :: (Alternative f) => (a -> Bool) -> a -> f a
guarded p x = bool empty (pure x) (p x)

guardedA :: (Functor f, Alternative t) => (a -> f Bool) -> a -> f (t a)
guardedA p x = bool empty (pure x) <$> p x

purer :: (Applicative f, Applicative g) => a -> f (g a)
purer = pure . pure

liftAA2 :: (Applicative f, Applicative g) => (a -> b -> c) -> f (g a) -> f (g b) -> f (g c)
liftAA2 = liftA2 . liftA2

(<<*>>) :: (Applicative f, Applicative g)  => f (g (a -> b)) -> f (g a) -> f (g b)
(<<*>>) = liftA2 (<*>)

