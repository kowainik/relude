{-# LANGUAGE Safe #-}

-- | Utility functions to work with 'Data.Maybe' data type as monad.

module Universum.Monad.Maybe
       ( module Data.Maybe

       , maybeToMonoid
       , whenJust
       , whenJustM
       , whenNothing
       , whenNothing_
       , whenNothingM
       , whenNothingM_
       ) where

import Data.Maybe (Maybe (..), catMaybes, fromMaybe, isJust, isNothing, mapMaybe, maybe,
                   maybeToList)

import Control.Applicative (Applicative, pure)
import Control.Monad (Monad (..))
import Data.Monoid (Monoid (mempty))

import Universum.Applicative (pass)

-- | Extracts 'Monoid' value from 'Maybe' returning 'mempty' if 'Nothing'.
--
-- >>> maybeToMonoid (Just [1,2,3] :: Maybe [Int])
-- [1,2,3]
-- >>> maybeToMonoid (Nothing :: Maybe [Int])
-- []
maybeToMonoid :: Monoid m => Maybe m -> m
maybeToMonoid = fromMaybe mempty

-- | Specialized version of 'for_' for 'Maybe'. It's used for code readability.
-- Also helps to avoid space leaks:
-- <http://www.snoyman.com/blog/2017/01/foldable-mapm-maybe-and-recursive-functions Foldable.mapM_ space leak>.
whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust (Just x) f = f x
whenJust Nothing _  = pass
{-# INLINE whenJust #-}

-- | Monadic version of 'whenJust'.
whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM mm f = mm >>= \m -> whenJust m f
{-# INLINE whenJustM #-}

-- | Performs default 'Applicative' action if 'Nothing' is given.
-- Otherwise returns content of 'Just' pured to 'Applicative'.
--
-- >>> whenNothing Nothing [True, False]
-- [True,False]
-- >>> whenNothing (Just True) [True, False]
-- [True]
whenNothing :: Applicative f => Maybe a -> f a -> f a
whenNothing (Just x) _ = pure x
whenNothing Nothing  m = m
{-# INLINE whenNothing #-}

-- | Performs default 'Applicative' action if 'Nothing' is given.
-- Do nothing for 'Just'. Convenient for discarding 'Just' content.
--
-- >>> whenNothing_ Nothing $ putText "Nothing!"
-- Nothing!
-- >>> whenNothing_ (Just True) $ putText "Nothing!"
whenNothing_ :: Applicative f => Maybe a -> f () -> f ()
whenNothing_ Nothing m = m
whenNothing_ _       _ = pass
{-# INLINE whenNothing_ #-}

-- | Monadic version of 'whenNothing'.
whenNothingM :: Monad m => m (Maybe a) -> m a -> m a
whenNothingM mm action = mm >>= \m -> whenNothing m action
{-# INLINE whenNothingM #-}

-- | Monadic version of 'whenNothingM_'.
whenNothingM_ :: Monad m => m (Maybe a) -> m () -> m ()
whenNothingM_ mm action = mm >>= \m -> whenNothing_ m action
{-# INLINE whenNothingM_ #-}
