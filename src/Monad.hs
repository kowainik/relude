{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy       #-}

module Monad
       ( Monad ((>>=), return)
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

       , guard
       , when
       , unless

       , whenJust
       , whenJustM

       , allM
       , anyM
       , andM
       , orM

       , liftM
       , liftM2
       , liftM3
       , liftM4
       , liftM5
       , liftM'
       , liftM2'
       , ap

       , (<$!>)
       ) where

import           Base                            (IO, seq)
import           Control.Applicative             (Applicative, pure)
import           Data.List                       (concat)
import           Data.Maybe                      (Maybe (..), maybe)
import           Prelude                         (Bool (..))

#if __GLASGOW_HASKELL__ >= 710
import           Control.Monad                   hiding (fail, (<$!>))
#else
import           Control.Monad                   hiding (fail)
#endif

#if __GLASGOW_HASKELL__ >= 800
import           Control.Monad.Fail              (MonadFail (..))
#else
import           Prelude                         (String)
import qualified Prelude                         as P (fail)
import           Text.ParserCombinators.ReadP
import           Text.ParserCombinators.ReadPrec
#endif

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)

liftM' :: Monad m => (a -> b) -> m a -> m b
liftM' = (<$!>)
{-# INLINE liftM' #-}

liftM2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2' f a b = do
  x <- a
  y <- b
  let z = f x y
  z `seq` return z
{-# INLINE liftM2' #-}

(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> m = do
  x <- m
  let z = f x
  z `seq` return z
{-# INLINE (<$!>) #-}

whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust (Just x) f = f x
whenJust Nothing _  = pure ()
{-# INLINE whenJust #-}

whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM mm f = maybe (return ()) f =<< mm
{-# INLINE whenJustM #-}

-- Copied from 'monad-loops' by James Cook (the library is in public domain)

andM :: (Monad m) => [m Bool] -> m Bool
andM []     = return True
andM (p:ps) = do
  q <- p
  if q then andM ps else return False

orM :: (Monad m) => [m Bool] -> m Bool
orM []     = return False
orM (p:ps) = do
  q <- p
  if q then return True else orM ps

anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM _ []     = return False
anyM p (x:xs) = do
  q <- p x
  if q then return True else anyM p xs

allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM _ []     = return True
allM p (x:xs) = do
  q <- p x
  if q then allM p xs else return False

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
