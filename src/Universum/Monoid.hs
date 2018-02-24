{-# LANGUAGE CPP #-}

module Universum.Monoid
       ( module Data.Monoid
#if ( __GLASGOW_HASKELL__ >= 800 )
       , module Data.Semigroup
#endif
       , maybeToMonoid
       ) where

import Data.Monoid (All (..), Alt (..), Any (..), Dual (..), Endo (..), First (..), Last (..),
                    Monoid (..), Product (..), Sum (..))

#if ( __GLASGOW_HASKELL__ >= 800 )
import Data.Semigroup (Option (..), Semigroup (sconcat, stimes, (<>)), WrappedMonoid, cycle1,
                       mtimesDefault, stimesIdempotent, stimesIdempotentMonoid, stimesMonoid)
#endif

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
