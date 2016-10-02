{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe              #-}

module Either
       ( maybeToLeft
       , maybeToRight
       , leftToMaybe
       , rightToMaybe
       , maybeToEither
       ) where

import           Data.Either   (Either (..), either)
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

maybeToEither :: Monoid b => (a -> b) -> Maybe a -> b
maybeToEither = maybe mempty
