{-# LANGUAGE Safe #-}

{- |
Copyright: (c) 2016 Stephen Diehl
           (c) 20016-2018 Serokell
           (c) 2018 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Monad transformers utilities.
-}

module Relude.Monad.Trans
       ( -- * Convenient functions to work with 'Reader' monad
         usingReader
       , usingReaderT

         -- * Convenient functions to work with 'State' monad
       , evaluatingState
       , evaluatingStateT
       , executingState
       , executingStateT
       , usingState
       , usingStateT
       ) where

import Prelude (flip, fst, snd)

import Relude.Functor (Functor, (<$>))
import Relude.Monad.Reexport (Reader, ReaderT, State, StateT, runReader, runReaderT, runState,
                              runStateT)

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
