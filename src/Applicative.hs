module Applicative (
  orAlt,
  orEmpty,
  eitherA,
) where

import Data.Monoid
import Control.Applicative
import Prelude (Bool, Either(..))

orAlt :: (Alternative f, Monoid a) => f a -> f a
orAlt f = f <|> pure mempty

orEmpty :: Alternative f => Bool -> a -> f a
orEmpty b a = if b then pure a else empty

eitherA :: (Alternative f) => f a -> f b -> f (Either a b)
eitherA a b = (Left <$> a) <|> (Right <$> b)
