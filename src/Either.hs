{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Either (
  maybeToLeft
, maybeToRight
, leftToMaybe
, rightToMaybe
, maybeToEither
) where

import Data.Function (const)
import Data.Monoid (Monoid, mempty)
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..), either)

leftToMaybe :: Either l r -> Maybe l
leftToMaybe = either Just (const Nothing)

rightToMaybe :: Either l r -> Maybe r
rightToMaybe = either (const Nothing) Just

maybeToRight :: l -> Maybe r -> Either l r
maybeToRight l = maybe (Left l) Right

maybeToLeft :: r -> Maybe l -> Either l r
maybeToLeft r = maybe (Right r) Left

maybeToEither :: Monoid b => (a -> b) -> Maybe a -> b
maybeToEither = maybe mempty
