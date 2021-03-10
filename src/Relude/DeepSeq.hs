{-# LANGUAGE Safe #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2021 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

This module contains useful functions to evaluate expressions to weak-head
normal form (WHNF) or just normal form (NF). Useful to force traces or @error@s
inside monadic computations or to remove space leaks.
-}

module Relude.DeepSeq
    ( -- * "Control.DeepSeq" reexports
      NFData (..)
    , deepseq
    , force
    , ($!!)

      -- * Evaluation
    , evaluateNF
    , evaluateNF_
    , evaluateWHNF
    , evaluateWHNF_
    ) where

import Control.DeepSeq (NFData (..), deepseq, force, ($!!))

import Relude.Base (IO, seq)
import Relude.Function ((.))
import Relude.Monad (MonadIO, liftIO, (<$!>))

import qualified Control.Exception.Base (evaluate)


-- $setup
-- >>> import Relude

{- | Lifted alias for 'Control.Exception.Base.evaluate' with a clearer name.

>>> let list = [1..5] :: [Int]
>>> :sprint list
list = _
>>> () <$ evaluateWHNF list
>>> :sprint list
list = 1 : _
-}
evaluateWHNF :: MonadIO m => a -> m a
evaluateWHNF = liftIO . Control.Exception.Base.evaluate
{-# INLINE evaluateWHNF #-}
{-# SPECIALIZE evaluateWHNF :: a -> IO a #-}

{- | Like 'evaluateWHNF' but discards value.

>>> let list = [1..5] :: [Int]
>>> :sprint list
list = _
>>> evaluateWHNF_ list
>>> :sprint list
list = 1 : _
-}
evaluateWHNF_ :: MonadIO m => a -> m ()
evaluateWHNF_ what = (`seq` ()) <$!> evaluateWHNF what
{-# INLINE evaluateWHNF_ #-}
{-# SPECIALIZE evaluateWHNF_ :: a -> IO () #-}

{- | Alias for @evaluateWHNF . force@ with a clearer name.

>>> let list = [1..5] :: [Int]
>>> :sprint list
list = _
>>> () <$ evaluateNF list
>>> :sprint list
list = [1,2,3,4,5]
-}
evaluateNF :: (NFData a, MonadIO m) => a -> m a
evaluateNF = evaluateWHNF . force
{-# INLINE evaluateNF #-}
{-# SPECIALIZE evaluateNF :: NFData a => a -> IO a #-}

{- | Alias for @evaluateWHNF . rnf@. Similar to 'evaluateNF'
but discards the resulting value.

>>> let list = [1..5] :: [Int]
>>> :sprint list
list = _
>>> evaluateNF_ list
>>> :sprint list
list = [1,2,3,4,5]
-}
evaluateNF_ :: (NFData a, MonadIO m) => a -> m ()
evaluateNF_ = evaluateWHNF . rnf
{-# INLINE evaluateNF_ #-}
{-# SPECIALIZE evaluateNF_ :: NFData a => a -> IO () #-}
