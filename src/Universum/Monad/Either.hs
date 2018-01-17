{-# LANGUAGE Safe #-}

-- | Utilites to work with @Either@ data type.

module Universum.Monad.Either
       ( fromLeft
       , fromRight
       , maybeToLeft
       , maybeToRight
       , leftToMaybe
       , rightToMaybe
       , whenLeft
       , whenLeftM
       , whenRight
       , whenRightM
       ) where

import Control.Applicative (Applicative)
import Control.Monad (Monad (..))
import Data.Function (const)
import Data.Maybe (Maybe (..), maybe)

import Universum.Applicative (pass)
import Universum.Monad.Reexport (Either (..), either)

-- $setup
-- >>> import Universum.Bool (Bool (..))

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

-- | Applies given action to 'Either' content if 'Left' is given.
whenLeft :: Applicative f => Either l r -> (l -> f ()) -> f ()
whenLeft (Left  l) f = f l
whenLeft (Right _) _ = pass
{-# INLINE whenLeft #-}

-- | Monadic version of 'whenLeft'.
whenLeftM :: Monad m => m (Either l r) -> (l -> m ()) -> m ()
whenLeftM me f = me >>= \e -> whenLeft e f
{-# INLINE whenLeftM #-}

-- | Applies given action to 'Either' content if 'Right' is given.
whenRight :: Applicative f => Either l r -> (r -> f ()) -> f ()
whenRight (Left  _) _ = pass
whenRight (Right r) f = f r
{-# INLINE whenRight #-}

-- | Monadic version of 'whenRight'.
whenRightM :: Monad m => m (Either l r) -> (r -> m ()) -> m ()
whenRightM me f = me >>= \e -> whenRight e f
{-# INLINE whenRightM #-}
