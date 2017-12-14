{-# LANGUAGE Safe #-}

-- | Convenient utils to work with 'Applicative'. There were more functions in this module
-- (see <https://www.stackage.org/haddock/lts-8.9/protolude-0.1.10/Applicative.html protolude version>)
-- but only convenient ans most used are left.

module Universum.Applicative
       ( module Control.Applicative
       , pass
       ) where

import Control.Applicative (Alternative (..), Applicative (..), Const (..), ZipList (..), liftA2,
                            liftA3, optional, (<**>))

-- | Shorter alias for @pure ()@.
--
-- >>> pass :: Maybe ()
-- Just ()
pass :: Applicative f => f ()
pass = pure ()

{-
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
-}
