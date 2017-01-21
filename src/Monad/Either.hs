{-# LANGUAGE Safe #-}

-- | Utilites to work with @Either@ data type.

module Monad.Either
       ( module Data.Either
       , maybeToLeft
       , maybeToRight
       , leftToMaybe
       , rightToMaybe
       , maybeToEither
       ) where

import           Data.Either   (Either (..), either, isLeft, isRight, lefts,
                                partitionEithers, rights)
import           Data.Function (const)
import           Data.Maybe    (Maybe (..), maybe)
import           Data.Monoid   (Monoid, mempty)

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
