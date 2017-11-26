{-# LANGUAGE CPP          #-}
{-# LANGUAGE Trustworthy  #-}
{-# LANGUAGE TypeFamilies #-}

-- | Reexporting useful monadic stuff.

module Monad
       ( module Monad.Maybe
       , module Monad.Either
       , module Monad.Trans

       , Monad ((>>=), (>>), return)
       , MonadFail (fail)
       , MonadPlus (..)

       , (=<<)
       , (>=>)
       , (<=<)
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

import Monad.Either
import Monad.Maybe
import Monad.Trans

import Base (IO, seq)
import Control.Applicative (Applicative (pure))
import Data.Function ((.))
import Data.Functor (fmap)
import Data.Traversable (Traversable (traverse))
import Prelude (Bool (..), Monoid, flip)

#if __GLASGOW_HASKELL__ >= 710
import Control.Monad hiding (fail, (<$!>))
#else
import Control.Monad hiding (fail)
#endif

#if __GLASGOW_HASKELL__ >= 800
import Control.Monad.Fail (MonadFail (..))
#else
import Prelude (Maybe (Nothing), String)
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadPrec (ReadPrec)

import qualified Prelude as P (fail)
#endif

import Container (Container, Element, fold, toList)

-- | Lifting bind into a monad. Generalized version of @concatMap@
-- that works with a monadic predicate. Old and simpler specialized to list
-- version had next type:
--
-- @
--     concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
-- @
--
-- Side note: previously it had type
--
-- @
--     concatMapM :: (Applicative q, Monad m, Traversable m)
--                => (a -> q (m b)) -> m a -> q (m b)
-- @
--
-- Such signature didn't allow to use this function when traversed container
-- type and type of returned by function-argument differed.
-- Now you can use it like e.g.
--
-- @
--     concatMapM readFile files >>= putStrLn
-- @
concatMapM
    :: ( Applicative f
       , Monoid m
       , Container (l m)
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
       , Traversable l
       )
    => l a -> (a -> f m) -> f m
concatForM = flip concatMapM
{-# INLINE concatForM #-}

-- | Stricter version of 'Data.Functor.<$>'.
(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> m = do
  x <- m
  let z = f x
  z `seq` return z
{-# INLINE (<$!>) #-}

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
-- >>> andM [putStrLn "1" >> pure True, putStrLn "2" >> pure False, putStrLn "3" >> undefined]
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


#if __GLASGOW_HASKELL__ < 800
-- | Class for 'Monad's that can 'fail'.
-- Copied from 'fail' by Herbert Valerio Riedel (the library is under BSD3).
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
