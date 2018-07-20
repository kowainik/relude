{-
Copyright: (c) 2016 Stephen Diehl
           (c) 20016-2018 Serokell
           (c) 2018 Kowainik
License: MIT
-}

-- | This module reexports functions to work with monoids plus adds extra useful functions.

module Universum.Monoid
       ( module Data.Monoid
       , module Data.Semigroup
       , maybeToMonoid
       ) where

import Data.Monoid (All (..), Alt (..), Any (..), Dual (..), Endo (..), First (..), Last (..),
                    Monoid (..), Product (..), Sum (..))
import Data.Semigroup (Option (..), Semigroup (sconcat, stimes, (<>)), WrappedMonoid, cycle1,
                       mtimesDefault, stimesIdempotent, stimesIdempotentMonoid, stimesMonoid)

import Universum.Monad.Reexport (Maybe, fromMaybe)

-- $setup
-- >>> import Universum.Base (Int)
-- >>> import Universum.Monad (Maybe (..))

-- | Extracts 'Monoid' value from 'Maybe' returning 'mempty' if 'Nothing'.
--
-- >>> maybeToMonoid (Just [1,2,3] :: Maybe [Int])
-- [1,2,3]
-- >>> maybeToMonoid (Nothing :: Maybe [Int])
-- []
maybeToMonoid :: Monoid m => Maybe m -> m
maybeToMonoid = fromMaybe mempty
