{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy       #-}
{-# LANGUAGE TypeFamilies      #-}

module Monad
       ( module Export

       , Monad ((>>=), return)
       , MonadFail (fail)
       , MonadPlus (..)

       , (=<<)
       , (>=>)
       , (<=<)
       , (>>)
       , forever

       , join
       , mfilter
       , filterM
       , mapAndUnzipM
       , zipWithM
       , zipWithM_
       , foldM
       , foldM_
       , replicateM
       , replicateM_

       , concatMapM
       , concatForM

       , guard
       , when
       , unless

       , allM
       , anyM
       , andM
       , orM

       , liftM
       , liftM2
       , liftM3
       , liftM4
       , liftM5
       , ap

       , (<$!>)
       ) where

import           Monad.Either                    as Export
import           Monad.Maybe                     as Export
import           Monad.Trans                     as Export

import           Base                            (IO, seq)
import           Control.Applicative             (Applicative (pure))
import           Data.Function                   ((.))
import           Data.Functor                    (fmap)
import           Data.Traversable                (Traversable (traverse))
import           Prelude                         (Bool (..), flip)

#if __GLASGOW_HASKELL__ >= 710
import           Control.Monad                   hiding (fail, (<$!>))
#else
import           Control.Monad                   hiding (fail)
#endif

#if __GLASGOW_HASKELL__ >= 800
import           Control.Monad.Fail              (MonadFail (..))
#else
import           Prelude                         (Maybe (Nothing), String)
import qualified Prelude                         as P (fail)
import           Text.ParserCombinators.ReadP    (ReadP)
import           Text.ParserCombinators.ReadPrec (ReadPrec)
#endif

import           Containers                      (Element, NontrivialContainer, toList)

-- old specialized to list version
-- concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
-- | Lifting bind into a monad. Generalized version of @concatMap@
-- that works with a monadic predicate.
concatMapM :: (Applicative q, Monad m, Traversable m)
           => (a -> q (m b))
           -> m a
           -> q (m b)
concatMapM f = fmap join . traverse f
{-# INLINE concatMapM #-}

-- | Like 'concatMapM', but has its arguments flipped, so can be used
-- instead of the common @fmap concat $ forM@ pattern.
concatForM :: (Applicative q, Monad m, Traversable m)
           => m a
           -> (a -> q (m b))
           -> q (m b)
concatForM = flip concatMapM
{-# INLINE concatForM #-}

(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> m = do
  x <- m
  let z = f x
  z `seq` return z
{-# INLINE (<$!>) #-}

andM :: (NontrivialContainer f, Element f ~ m Bool, Monad m) => f -> m Bool
andM = go . toList
  where
    go []     = pure True
    go (p:ps) = do
        q <- p
        if q then go ps else pure False

orM :: (NontrivialContainer f, Element f ~ m Bool, Monad m) => f -> m Bool
orM = go . toList
  where
    go []     = pure False
    go (p:ps) = do
        q <- p
        if q then pure True else go ps

allM :: (NontrivialContainer f, Monad m) => (Element f -> m Bool) -> f -> m Bool
allM p = go . toList
  where
    go []     = pure True
    go (x:xs) = do
        q <- p x
        if q then go xs else pure False

anyM :: (NontrivialContainer f, Monad m) => (Element f -> m Bool) -> f -> m Bool
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

-- Copied from 'fail' by Herbert Valerio Riedel (the library is under BSD3)

#if __GLASGOW_HASKELL__ < 800
class Monad m => MonadFail m where
    fail :: String -> m a

instance MonadFail Maybe where
    fail _ = Nothing

instance MonadFail [] where
    fail _ = []

instance MonadFail IO where
    fail = P.fail

instance MonadFail ReadPrec where
    fail = P.fail -- = P (\_ -> fail s)

instance MonadFail ReadP where
    fail = P.fail
#endif
