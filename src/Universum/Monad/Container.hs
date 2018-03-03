{-# LANGUAGE TypeFamilies #-}

-- | This module exports functions which allow to process instances of
-- 'Container' type class in monadic way.

module Universum.Monad.Container
       ( concatMapM
       , concatForM

       , allM
       , anyM
       , andM
       , orM
       ) where

import Control.Applicative (Applicative (pure))
import Data.Function ((.))
import Data.Traversable (Traversable (traverse))
import Prelude (Bool (..), Monoid, flip)

import Universum.Base (IO)
import Universum.Container (Container, Element, fold, toList)
import Universum.Functor (fmap)
import Universum.Monad.Reexport (Monad (..))

-- $setup
-- :set -XOverloadedStrings
-- >>> import Universum.Base (even)
-- >>> import Universum.Monad (Maybe (..), (>=>))
-- >>> import Universum.Print (putTextLn)
-- >>> import Universum.String (readMaybe)

-- | Lifting bind into a monad. Generalized version of @concatMap@
-- that works with a monadic predicate. Old and simpler specialized to list
-- version had next type:
--
-- @
-- concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
-- @
--
-- Side note: previously it had type
--
-- @
-- concatMapM :: (Applicative q, Monad m, Traversable m)
--            => (a -> q (m b)) -> m a -> q (m b)
-- @
--
-- Such signature didn't allow to use this function when traversed container
-- type and type of returned by function-argument differed.
-- Now you can use it like e.g.
--
-- @
-- concatMapM readFile files >>= putTextLn
-- @
concatMapM
    :: ( Applicative f
       , Monoid m
       , Container (l m)
       , Element (l m) ~ m
       , Traversable l
       )
    => (a -> f m) -> l a -> f m
concatMapM f = fmap fold . traverse f
{-# INLINE concatMapM #-}

-- | Like 'concatMapM', but has its arguments flipped, so can be used
-- instead of the common @fmap concat $ forM@ pattern.
concatForM
    :: ( Applicative f
       , Monoid m
       , Container (l m)
       , Element (l m) ~ m
       , Traversable l
       )
    => l a -> (a -> f m) -> f m
concatForM = flip concatMapM
{-# INLINE concatForM #-}

-- | Monadic and constrained to 'Container' version of 'Prelude.and'.
--
-- >>> andM [Just True, Just False]
-- Just False
-- >>> andM [Just True]
-- Just True
-- >>> andM [Just True, Just False, Nothing]
-- Just False
-- >>> andM [Just True, Nothing]
-- Nothing
-- >>> andM [putTextLn "1" >> pure True, putTextLn "2" >> pure False, putTextLn "3" >> pure True]
-- 1
-- 2
-- False
andM :: (Container f, Element f ~ m Bool, Monad m) => f -> m Bool
andM = go . toList
  where
    go []     = pure True
    go (p:ps) = do
        q <- p
        if q then go ps else pure False

-- | Monadic and constrained to 'Container' version of 'Prelude.or'.
--
-- >>> orM [Just True, Just False]
-- Just True
-- >>> orM [Just True, Nothing]
-- Just True
-- >>> orM [Nothing, Just True]
-- Nothing
orM :: (Container f, Element f ~ m Bool, Monad m) => f -> m Bool
orM = go . toList
  where
    go []     = pure False
    go (p:ps) = do
        q <- p
        if q then pure True else go ps

-- | Monadic and constrained to 'Container' version of 'Prelude.all'.
--
-- >>> allM (readMaybe >=> pure . even) ["6", "10"]
-- Just True
-- >>> allM (readMaybe >=> pure . even) ["5", "aba"]
-- Just False
-- >>> allM (readMaybe >=> pure . even) ["aba", "10"]
-- Nothing
allM :: (Container f, Monad m) => (Element f -> m Bool) -> f -> m Bool
allM p = go . toList
  where
    go []     = pure True
    go (x:xs) = do
        q <- p x
        if q then go xs else pure False

-- | Monadic and constrained to 'Container' version of 'Prelude.any'.
--
-- >>> anyM (readMaybe >=> pure . even) ["5", "10"]
-- Just True
-- >>> anyM (readMaybe >=> pure . even) ["10", "aba"]
-- Just True
-- >>> anyM (readMaybe >=> pure . even) ["aba", "10"]
-- Nothing
anyM :: (Container f, Monad m) => (Element f -> m Bool) -> f -> m Bool
anyM p = go . toList
  where
    go []     = pure False
    go (x:xs) = do
        q <- p x
        if q then pure True else go xs

{-# SPECIALIZE andM :: [IO Bool] -> IO Bool #-}
{-# SPECIALIZE orM  :: [IO Bool] -> IO Bool #-}
{-# SPECIALIZE anyM :: (a -> IO Bool) -> [a] -> IO Bool #-}
{-# SPECIALIZE allM :: (a -> IO Bool) -> [a] -> IO Bool #-}
