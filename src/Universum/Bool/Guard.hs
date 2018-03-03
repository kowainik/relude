-- | This module contains monadic predicates.

module Universum.Bool.Guard
       ( guardM
       , ifM
       , unlessM
       , whenM
       ) where

import Universum.Bool.Reexport (Bool, guard, unless, when)
import Universum.Function (flip)
import Universum.Monad (Monad, MonadPlus, (>>=))

-- $setup
-- >>> import Universum.Applicative (pure)
-- >>> import Universum.Bool.Reexport (Bool (..))
-- >>> import Universum.Function (($))
-- >>> import Universum.Monad (Maybe (..))
-- >>> import Universum.Print (putTextLn)

-- | Monadic version of 'when'.
--
-- >>> whenM (pure False) $ putTextLn "No text :("
-- >>> whenM (pure True)  $ putTextLn "Yes text :)"
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
-- >>> unlessM (pure False) $ putTextLn "No text :("
-- No text :(
-- >>> unlessM (pure True) $ putTextLn "Yes text :)"
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p m = p >>= flip unless m
{-# INLINE unlessM #-}

-- | Monadic version of @if-then-else@.
--
-- >>> ifM (pure True) (putTextLn "True text") (putTextLn "False text")
-- True text
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p x y = p >>= \b -> if b then x else y
{-# INLINE ifM #-}

-- | Monadic version of 'guard'. Occasionally useful.
-- Here some complex but real-life example:
--
-- @
-- findSomePath :: IO (Maybe FilePath)
--
-- somePath :: MaybeT IO FilePath
-- somePath = do
--     path <- MaybeT findSomePath
--     guardM $ liftIO $ doesDirectoryExist path
--     return path
-- @
guardM :: MonadPlus m => m Bool -> m ()
guardM f = f >>= guard
{-# INLINE guardM #-}
