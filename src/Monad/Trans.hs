{-# LANGUAGE Safe #-}

-- | Monad transformers utilities.

module Monad.Trans
       ( module X

       , usingReader
       , usingReaderT

       , evaluatingState
       , evaluatingStateT
       , executingState
       , executingStateT
       , usingState
       , usingStateT
       ) where

-- Monad transformers
import           Control.Monad.Catch  as X (MonadCatch (catch), MonadMask (..),
                                            MonadThrow (throwM), bracket, bracket_,
                                            catchAll, finally)
import           Control.Monad.Except as X (ExceptT (..), runExceptT)
import           Control.Monad.State  as X (MonadState, State, StateT (..), evalState,
                                            evalStateT, execState, execStateT, gets,
                                            modify, runState, state, withState)

import           Control.Monad.Reader as X (MonadReader, Reader, ReaderT (..), ask, asks,
                                            local, reader, runReader)

import           Control.Monad.Trans  as X (MonadIO, lift, liftIO)

import           Prelude              (Functor, flip, fst, snd, (<$>))

usingReaderT :: r -> ReaderT r m a -> m a
usingReaderT = flip runReaderT
{-# INLINE usingReaderT #-}

usingReader :: r -> Reader r a -> a
usingReader = flip runReader
{-# INLINE usingReader #-}

usingStateT :: s -> StateT s m a -> m (a, s)
usingStateT = flip runStateT
{-# INLINE usingStateT #-}

usingState :: s -> State s a -> (a, s)
usingState = flip runState
{-# INLINE usingState #-}

evaluatingStateT :: Functor f => s -> StateT s f a -> f a
evaluatingStateT s st = fst <$> usingStateT s st
{-# INLINE evaluatingStateT #-}

evaluatingState :: s -> State s a -> a
evaluatingState s st = fst (usingState s st)
{-# INLINE evaluatingState #-}

executingStateT :: Functor f => s -> StateT s f a -> f s
executingStateT s st = snd <$> usingStateT s st
{-# INLINE executingStateT #-}

executingState :: s -> State s a -> s
executingState s st = snd (usingState s st)
{-# INLINE executingState #-}
