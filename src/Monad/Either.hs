{-# LANGUAGE Safe #-}

-- | Utilites to work with @Either@ data type.

module Monad.Either
       ( module Data.Either
       , maybeToLeft
       , maybeToRight
       , leftToMaybe
       , rightToMaybe
       , maybeToEither
       , whenLeft
       , whenLeftM
       , whenRight
       , whenRightM
       ) where

import           Control.Applicative (Applicative)
import           Control.Monad       (Monad (..))
import           Data.Either         (Either (..), either, isLeft, isRight, lefts,
                                      partitionEithers, rights)
import           Data.Function       (const)
import           Data.Maybe          (Maybe (..), maybe)
import           Data.Monoid         (Monoid, mempty)

import           Applicative         (pass)

leftToMaybe :: Either l r -> Maybe l
leftToMaybe = either Just (const Nothing)

rightToMaybe :: Either l r -> Maybe r
rightToMaybe = either (const Nothing) Just

maybeToRight :: l -> Maybe r -> Either l r
maybeToRight l = maybe (Left l) Right

maybeToLeft :: r -> Maybe l -> Either l r
maybeToLeft r = maybe (Right r) Left

-- TODO: why this is @toEither@?
maybeToEither :: Monoid b => (a -> b) -> Maybe a -> b
maybeToEither = maybe mempty

whenLeft :: Applicative f => Either l r -> (l -> f ()) -> f ()
whenLeft (Left  l) f = f l
whenLeft (Right _) _ = pass
{-# INLINE whenLeft #-}

whenLeftM :: Monad m => m (Either l r) -> (l -> m ()) -> m ()
whenLeftM me f = me >>= \e -> whenLeft e f
{-# INLINE whenLeftM #-}

whenRight :: Applicative f => Either l r -> (r -> f ()) -> f ()
whenRight (Left  _) _ = pass
whenRight (Right r) f = f r
{-# INLINE whenRight #-}

whenRightM :: Monad m => m (Either l r) -> (r -> m ()) -> m ()
whenRightM me f = me >>= \e -> whenRight e f
{-# INLINE whenRightM #-}
