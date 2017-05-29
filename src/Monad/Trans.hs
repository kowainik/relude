{-# LANGUAGE Safe #-}

-- | Monad transformers utilities.

module Monad.Trans
       ( -- * Reexports from @Control.Monad.*@
         module Control.Monad.Catch
       , module Control.Monad.Except
       , module Control.Monad.Reader
       , module Control.Monad.State
       , module Control.Monad.Trans
       , module Control.Monad.Trans.Maybe

         -- * Convenient functions to work with 'Reader' monad
       , usingReader
       , usingReaderT

         -- * Convenient functions to work with 'State' monad
       , evaluatingState
       , evaluatingStateT
       , executingState
       , executingStateT
       , usingState
       , usingStateT
       ) where

-- Monad transformers
import           Control.Monad.Catch       (MonadCatch (catch), MonadMask (..),
                                            MonadThrow (throwM), bracket, bracket_,
                                            catchAll, finally)
import           Control.Monad.Except      (ExceptT (..), runExceptT)
import           Control.Monad.Reader      (MonadReader, Reader, ReaderT (..), ask, asks,
                                            local, reader, runReader)
import           Control.Monad.State       (MonadState, State, StateT (..), evalState,
                                            evalStateT, execState, execStateT, gets,
                                            modify, runState, state, withState)
import           Control.Monad.Trans       (MonadIO, MonadTrans, lift, liftIO)
import           Control.Monad.Trans.Maybe (MaybeT (..), exceptToMaybeT, maybeToExceptT)

import           Prelude                   (Functor, flip, fst, snd, (<$>))

-- | Shorter and more readable alias for @flip runReaderT@.
usingReaderT :: r -> ReaderT r m a -> m a
usingReaderT = flip runReaderT
{-# INLINE usingReaderT #-}

-- | Shorter and more readable alias for @flip runReader@.
usingReader :: r -> Reader r a -> a
usingReader = flip runReader
{-# INLINE usingReader #-}

-- | Shorter and more readable alias for @flip runStateT@.
usingStateT :: s -> StateT s m a -> m (a, s)
usingStateT = flip runStateT
{-# INLINE usingStateT #-}

-- | Shorter and more readable alias for @flip runState@.
usingState :: s -> State s a -> (a, s)
usingState = flip runState
{-# INLINE usingState #-}

-- | Alias for @flip evalStateT@. It's not shorter but sometimes
-- more readable. Done by analogy with @using*@ functions family.
evaluatingStateT :: Functor f => s -> StateT s f a -> f a
evaluatingStateT s st = fst <$> usingStateT s st
{-# INLINE evaluatingStateT #-}

-- | Alias for @flip evalState@. It's not shorter but sometimes
-- more readable. Done by analogy with @using*@ functions family.
evaluatingState :: s -> State s a -> a
evaluatingState s st = fst (usingState s st)
{-# INLINE evaluatingState #-}

-- | Alias for @flip execStateT@. It's not shorter but sometimes
-- more readable. Done by analogy with @using*@ functions family.
executingStateT :: Functor f => s -> StateT s f a -> f s
executingStateT s st = snd <$> usingStateT s st
{-# INLINE executingStateT #-}

-- | Alias for @flip execState@. It's not shorter but sometimes
-- more readable. Done by analogy with @using*@ functions family.
executingState :: s -> State s a -> s
executingState s st = snd (usingState s st)
{-# INLINE executingState #-}
