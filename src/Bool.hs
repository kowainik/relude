{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Bool (
  whenM
, unlessM
, ifM
, guardM
, bool
) where

import Data.Bool (Bool)
import Data.Function (flip)
import Control.Monad (Monad, MonadPlus, when, unless, guard, (>>=), (=<<))

bool :: a -> a -> Bool -> a
bool f t p = if p then t else f

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m =
  p >>= flip when m

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p m =
  p >>= flip unless m

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p x y = p >>= \b -> if b then x else y

guardM :: MonadPlus m => m Bool -> m ()
guardM f = guard =<< f
