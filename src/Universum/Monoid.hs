{-# LANGUAGE CPP #-}

module Universum.Monoid
       (
#if ( __GLASGOW_HASKELL__ >= 800 )
         module Data.List.NonEmpty
       , module Data.Monoid
       , module Data.Semigroup
#else
         module Data.Monoid
#endif
       , maybeToMonoid
       ) where

#if ( __GLASGOW_HASKELL__ >= 800 )
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Monoid hiding ((<>))
import Data.Semigroup (Option (..), Semigroup (sconcat, stimes, (<>)), WrappedMonoid, cycle1,
                            mtimesDefault, stimesIdempotent, stimesIdempotentMonoid, stimesMonoid)
#else
import Data.Monoid hiding ((<>))
#endif

import Universum.Monad.Reexport (Maybe, fromMaybe)

-- | Extracts 'Monoid' value from 'Maybe' returning 'mempty' if 'Nothing'.
--
-- >>> maybeToMonoid (Just [1,2,3] :: Maybe [Int])
-- [1,2,3]
-- >>> maybeToMonoid (Nothing :: Maybe [Int])
-- []
maybeToMonoid :: Monoid m => Maybe m -> m
maybeToMonoid = fromMaybe mempty
