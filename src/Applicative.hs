{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe              #-}

module Applicative
       ( orAlt
       , orEmpty
       , eitherA
       , liftAA2
       , purer
       , (<<*>>)
       ) where

import           Control.Applicative
import           Data.Bool           (Bool)
import           Data.Either         (Either (..))
import           Data.Function       ((.))
import           Data.Monoid         (Monoid (..))

orAlt :: (Alternative f, Monoid a) => f a -> f a
orAlt f = f <|> pure mempty

orEmpty :: Alternative f => Bool -> a -> f a
orEmpty b a = if b then pure a else empty

eitherA :: Alternative f => f a -> f b -> f (Either a b)
eitherA a b = (Left <$> a) <|> (Right <$> b)

purer :: (Applicative f, Applicative g) => a -> f (g a)
purer = pure . pure

liftAA2 :: (Applicative f, Applicative g) => (a -> b -> c) -> f (g a) -> f (g b) -> f (g c)
liftAA2 = liftA2 . liftA2

(<<*>>) :: (Applicative f, Applicative g)  => f (g (a -> b)) -> f (g a) -> f (g b)
(<<*>>) = liftA2 (<*>)
