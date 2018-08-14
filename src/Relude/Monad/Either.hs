{-# LANGUAGE CPP  #-}
{-# LANGUAGE Safe #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
Copyright: (c) 2016 Stephen Diehl
           (c) 20016-2018 Serokell
           (c) 2018 Kowainik
License: MIT
-}

-- | Utilites to work with @Either@ data type.

module Relude.Monad.Either
       ( fromLeft
       , fromRight
       , maybeToLeft
       , maybeToRight
       , leftToMaybe
       , rightToMaybe
       , whenLeft
       , whenLeft_
       , whenLeftM
       , whenLeftM_
       , whenRight
       , whenRight_
       , whenRightM
       , whenRightM_
       ) where

import Control.Applicative (Applicative)
import Control.Monad (Monad (..))
import Data.Function (const)
import Data.Maybe (Maybe (..), maybe)

import Relude.Applicative (pure)
import Relude.Function ((.))
import Relude.Monad.Reexport (Either (..), MonadFail (..), either)
import Relude.String (IsString, fromString)

#if MIN_VERSION_base(4,10,0)
import Data.Either (fromLeft, fromRight)
#else
-- | Extracts value from 'Left' or return given default value.
--
-- >>> fromLeft 0 (Left 3)
-- 3
-- >>> fromLeft 0 (Right 5)
-- 0
fromLeft :: a -> Either a b -> a
fromLeft _ (Left a)  = a
fromLeft a (Right _) = a

-- | Extracts value from 'Right' or return given default value.
--
-- >>> fromRight 0 (Left 3)
-- 0
-- >>> fromRight 0 (Right 5)
-- 5
fromRight :: b -> Either a b -> b
fromRight b (Left _)  = b
fromRight _ (Right b) = b
#endif

-- $setup
-- >>> import Relude.Bool (Bool (..))

instance IsString str => MonadFail (Either str) where
    fail = Left . fromString

-- | Maps left part of 'Either' to 'Maybe'.
--
-- >>> leftToMaybe (Left True)
-- Just True
-- >>> leftToMaybe (Right "aba")
-- Nothing
leftToMaybe :: Either l r -> Maybe l
leftToMaybe = either Just (const Nothing)

-- | Maps right part of 'Either' to 'Maybe'.
--
-- >>> rightToMaybe (Left True)
-- Nothing
-- >>> rightToMaybe (Right "aba")
-- Just "aba"
rightToMaybe :: Either l r -> Maybe r
rightToMaybe = either (const Nothing) Just

-- | Maps 'Maybe' to 'Either' wrapping default value into 'Left'.
--
-- >>> maybeToRight True (Just "aba")
-- Right "aba"
-- >>> maybeToRight True Nothing
-- Left True
maybeToRight :: l -> Maybe r -> Either l r
maybeToRight l = maybe (Left l) Right

-- | Maps 'Maybe' to 'Either' wrapping default value into 'Right'.
--
-- >>> maybeToLeft True (Just "aba")
-- Left "aba"
-- >>> maybeToLeft True Nothing
-- Right True
maybeToLeft :: r -> Maybe l -> Either l r
maybeToLeft r = maybe (Right r) Left

-- | Applies given action to 'Either' content if 'Left' is given and returns
-- the result. In case of 'Right' the default value will be returned.
whenLeft :: Applicative f => a -> Either l r -> (l -> f a) -> f a
whenLeft _ (Left  l) f = f l
whenLeft a (Right _) _ = pure a
{-# INLINE whenLeft #-}

-- | Applies given action to 'Either' content if 'Left' is given.
whenLeft_ :: Applicative f => Either l r -> (l -> f ()) -> f ()
whenLeft_ = whenLeft ()
{-# INLINE whenLeft_ #-}

-- | Monadic version of 'whenLeft'.
whenLeftM :: Monad m => a -> m (Either l r) -> (l -> m a) -> m a
whenLeftM a me f = me >>= \e -> whenLeft a e f
{-# INLINE whenLeftM #-}

-- | Monadic version of 'whenLeft_'.
whenLeftM_ :: Monad m => m (Either l r) -> (l -> m ()) -> m ()
whenLeftM_ me f = me >>= \e -> whenLeft_ e f
{-# INLINE whenLeftM_ #-}

-- | Applies given action to 'Either' content if 'Right' is given and returns
-- the result. In case of 'Left' the default value will be returned.
whenRight :: Applicative f => a -> Either l r -> (r -> f a) -> f a
whenRight a (Left  _) _ = pure a
whenRight _ (Right r) f = f r
{-# INLINE whenRight #-}

-- | Applies given action to 'Either' content if 'Right' is given.
whenRight_ :: Applicative f => Either l r -> (r -> f ()) -> f ()
whenRight_ = whenRight ()
{-# INLINE whenRight_ #-}

-- | Monadic version of 'whenRight'.
whenRightM :: Monad m => a -> m (Either l r) -> (r -> m a) -> m a
whenRightM a me f = me >>= \e -> whenRight a e f
{-# INLINE whenRightM #-}

-- | Monadic version of 'whenRight_'.
whenRightM_ :: Monad m => m (Either l r) -> (r -> m ()) -> m ()
whenRightM_ me f = me >>= \e -> whenRight_ e f
{-# INLINE whenRightM_ #-}
