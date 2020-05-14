{-# LANGUAGE CPP         #-}
{-# LANGUAGE Trustworthy #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2020 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

Monad transformers utilities.
-}

module Relude.Monad.Trans
    ( -- * Convenient functions to work with 'Reader' monad
      usingReader
    , usingReaderT

#if MIN_VERSION_base(4,10,0)
    , etaReaderT
#endif

      -- * Convenient functions to work with 'State' monad
    , evaluatingState
    , evaluatingStateT
    , executingState
    , executingStateT
    , usingState
    , usingStateT

      -- * Lifted to Transformers
    , hoistMaybe
    , hoistEither
    ) where

#if MIN_VERSION_base(4,10,0)
import GHC.Exts (oneShot)
#endif

import Relude.Applicative (Applicative (pure))
import Relude.Container.Reexport (fst, snd)
import Relude.Function (flip, (.))
import Relude.Functor (Functor, (<$>))
import Relude.Monad.Reexport (Either, ExceptT (..), Maybe, MaybeT (..), Reader, ReaderT (..), State,
                              StateT, runReader, runReaderT, runState, runStateT)


-- $setup
-- >>> import Relude

{- | Shorter and more readable alias for @flip runReaderT@.

>>> usingReaderT 42 $ asks (+5)
47
-}
usingReaderT :: r -> ReaderT r m a -> m a
usingReaderT = flip runReaderT
{-# INLINE usingReaderT #-}

{- | Shorter and more readable alias for @flip runReader@.

>>> usingReader 42 $ asks (+5)
47
-}
usingReader :: r -> Reader r a -> a
usingReader = flip runReader
{-# INLINE usingReader #-}

#if MIN_VERSION_base(4,10,0)
{- | This function helps with optimizing performance when working with
the 'ReaderT' transformer. If you have code like below, that is
called in a loop

@
step :: Instruction -> 'ReaderT' Config IO Result
step instruction = __case__ instruction __of__
    Add -> __do__ stuff ...
    Del -> __do__ stuff ...
@

you can improve performance of your Haskell applications by using
'etaReaderT' in the following way:

@
step :: Instruction -> 'ReaderT' Config IO Result
step instruction = 'etaReaderT' $ __case__ instruction __of__
    Add -> __do__ stuff ...
    Del -> __do__ stuff ...
@

For a detailed explanation, refer to the following blog post:

* <https://www.joachim-breitner.de/blog/763-Faster_Winter_5__Eta-Expanding_ReaderT Faster Winter 5: Eta-Expanding ReaderT (by Joachim Breitners)>

@since 0.7.0.0
-}
etaReaderT :: ReaderT r m a -> ReaderT r m a
etaReaderT = ReaderT . oneShot . runReaderT
{-# INLINE etaReaderT #-}
#endif

{- | Shorter and more readable alias for @flip runStateT@.

>>> usingStateT 0 $ put 42 >> pure False
(False,42)
-}
usingStateT :: s -> StateT s m a -> m (a, s)
usingStateT = flip runStateT
{-# INLINE usingStateT #-}

-- | Shorter and more readable alias for @flip runState@.
usingState :: s -> State s a -> (a, s)
usingState = flip runState
{-# INLINE usingState #-}

{- | Alias for @flip evalStateT@. It's not shorter but sometimes
more readable. Done by analogy with @using*@ functions family.
-}
evaluatingStateT :: Functor f => s -> StateT s f a -> f a
evaluatingStateT s st = fst <$> usingStateT s st
{-# INLINE evaluatingStateT #-}

{- | Alias for @flip evalState@. It's not shorter but sometimes
more readable. Done by analogy with @using*@ functions family.
-}
evaluatingState :: s -> State s a -> a
evaluatingState s st = fst (usingState s st)
{-# INLINE evaluatingState #-}

{- | Alias for @flip execStateT@. It's not shorter but sometimes
more readable. Done by analogy with @using*@ functions family.
-}
executingStateT :: Functor f => s -> StateT s f a -> f s
executingStateT s st = snd <$> usingStateT s st
{-# INLINE executingStateT #-}

{- | Alias for @flip execState@. It's not shorter but sometimes
more readable. Done by analogy with @using*@ functions family.
-}
executingState :: s -> State s a -> s
executingState s st = snd (usingState s st)
{-# INLINE executingState #-}

{- | Lift a 'Maybe' to the 'MaybeT' monad

@since 0.3.0
-}
hoistMaybe  :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe m = MaybeT (pure m)
{-# INLINE hoistMaybe #-}

{- | Lift a 'Either' to the 'ExceptT' monad

@since 0.3.0
-}
hoistEither :: Applicative m => Either e a -> ExceptT e m a
hoistEither e = ExceptT (pure e)
{-# INLINE hoistEither #-}
