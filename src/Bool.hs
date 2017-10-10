{-# LANGUAGE Safe #-}

-- | Convenient commonly used and very helpful functions to work with
-- 'Bool' and also with monads.

module Bool
       ( bool
       , guard
       , guardM
       , ifM
       , unless
       , unlessM
       , when
       , whenM
       ) where

import           Control.Monad (Monad, MonadPlus, guard, unless, when, (>>=))
import           Data.Bool     (Bool)
import           Data.Function (flip)

-- | Reversed version of @if-then-else@.
--
-- >>> bool 5 10 True
-- 10
-- >>> bool 5 10 False
-- 5
bool :: a -> a -> Bool -> a
bool f t p = if p then t else f

-- | Monadic version of 'when'.
--
-- >>> whenM (pure False) $ putText "No text :("
-- >>> whenM (pure True)  $ putText "Yes text :)"
-- Yes text :)
-- >>> whenM (Just True) (pure ())
-- Just ()
-- >>> whenM (Just False) (pure ())
-- Just ()
-- >>> whenM Nothing (pure ())
-- Nothing
whenM :: Monad m => m Bool -> m () -> m ()
whenM p m = p >>= flip when m
{-# INLINE whenM #-}

-- | Monadic version of 'unless'.
--
-- >>> unlessM (pure False) $ putText "No text :("
-- No text :(
-- >>> unlessM (pure True) $ putText "Yes text :)"
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p m = p >>= flip unless m
{-# INLINE unlessM #-}

-- | Monadic version of @if-then-else@.
--
-- >>> ifM (pure True) (putText "True text") (putText "False text")
-- True text
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p x y = p >>= \b -> if b then x else y
{-# INLINE ifM #-}

-- | Monadic version of 'guard'. Occasionally useful.
-- Here some complex but real-life example:
-- @
--   findSomePath :: IO (Maybe FilePath)
--
--   somePath :: MaybeT IO FilePath
--   somePath = do
--       path <- MaybeT findSomePath
--       guardM $ liftIO $ doesDirectoryExist path
--       return path
-- @
guardM :: MonadPlus m => m Bool -> m ()
guardM f = f >>= guard
{-# INLINE guardM #-}
