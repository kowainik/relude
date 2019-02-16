{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Reexports functions to work with monoids plus adds extra useful functions.
-}

module Relude.Monoid
       ( module Data.Monoid
       , module Data.Semigroup

       , Ap (..)
       , maybeToMonoid
       ) where

#if MIN_VERSION_base(4,12,0)
import Data.Monoid (Ap (..))
#endif
import Data.Monoid (All (..), Alt (..), Any (..), Dual (..), Endo (..), First (..), Last (..),
                    Monoid (..), Product (..), Sum (..))
import Data.Semigroup (Option (..), Semigroup (sconcat, stimes, (<>)), WrappedMonoid, cycle1,
                       mtimesDefault, stimesIdempotent, stimesIdempotentMonoid, stimesMonoid)

import Relude.Monad.Reexport (Maybe, fromMaybe)

#if !MIN_VERSION_base(4,12,0)
import GHC.Generics (Generic1)

import Relude.Applicative (Alternative, Applicative (..))
import Relude.Base (Bounded (..), Enum, Eq, Generic, Ord, Show)
import Relude.Function (($), (.))
import Relude.Functor.Reexport (Functor (..))
import Relude.Monad.Reexport (Monad, MonadFail, MonadPlus)
import Relude.Numeric (Num (..))
import Relude.String.Reexport (Read)
#endif


-- $setup
-- >>> import Relude.Monad (Maybe (..))
-- >>> import Relude.Numeric (Int)

-- | Extracts 'Monoid' value from 'Maybe' returning 'mempty' if 'Nothing'.
--
-- >>> maybeToMonoid (Just [1,2,3] :: Maybe [Int])
-- [1,2,3]
-- >>> maybeToMonoid (Nothing :: Maybe [Int])
-- []
maybeToMonoid :: Monoid m => Maybe m -> m
maybeToMonoid = fromMaybe mempty

#if !MIN_VERSION_base(4,12,0)
-- | This data type witnesses the lifting of a 'Monoid' into an
-- 'Applicative' pointwise.
--
-- @since 4.12.0.0
newtype Ap f a = Ap { getAp :: f a }
    deriving ( Alternative -- ^ @since 4.12.0.0
             , Applicative -- ^ @since 4.12.0.0
             , Enum        -- ^ @since 4.12.0.0
             , Eq          -- ^ @since 4.12.0.0
             , Functor     -- ^ @since 4.12.0.0
             , Generic     -- ^ @since 4.12.0.0
             , Generic1    -- ^ @since 4.12.0.0
             , Monad       -- ^ @since 4.12.0.0
             , MonadFail   -- ^ @since 4.12.0.0
             , MonadPlus   -- ^ @since 4.12.0.0
             , Ord         -- ^ @since 4.12.0.0
             , Read        -- ^ @since 4.12.0.0
             , Show        -- ^ @since 4.12.0.0
             )

-- | @since 4.12.0.0
instance (Applicative f, Semigroup a) => Semigroup (Ap f a) where
        (Ap x) <> (Ap y) = Ap $ liftA2 (<>) x y

-- | @since 4.12.0.0
instance (Applicative f, Monoid a) => Monoid (Ap f a) where
        mempty = Ap $ pure mempty

-- | @since 4.12.0.0
instance (Applicative f, Bounded a) => Bounded (Ap f a) where
  minBound = pure minBound
  maxBound = pure maxBound

-- | @since 4.12.0.0
instance (Applicative f, Num a) => Num (Ap f a) where
  (+)         = liftA2 (+)
  (*)         = liftA2 (*)
  negate      = fmap negate
  fromInteger = pure . fromInteger
  abs         = fmap abs
  signum      = fmap signum
#endif
