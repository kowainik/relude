{-# LANGUAGE CPP #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2019 Kowainik
SPDX-License-Identifier: MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module contains useful functions to evaluate expressions to weak-head
normal form or just normal form. Useful to force traces or @error@ inside
monadic computation or to remove space leaks.
-}

module Relude.DeepSeq
       ( module Control.DeepSeq
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

{- | Lifted alias for 'Control.Exception.Base.evaluate' with clearer name.

>>> list = [1..5] :: [Int]
>>> :sprint list
list = _
>>> () <$ evaluateWHNF list
#if MIN_VERSION_base(4,13,0)
>>> :sprint list
list = 1 : _
#else
>>> :sprint list
list = _ : _
#endif
-}
evaluateWHNF :: MonadIO m => a -> m a
evaluateWHNF = liftIO . Control.Exception.Base.evaluate
{-# INLINE evaluateWHNF #-}
{-# SPECIALIZE evaluateWHNF :: a -> IO a #-}

{- | Like 'evaluateWHNF' but discards value.

>>> list = [1..5] :: [Int]
>>> :sprint list
list = _
>>> evaluateWHNF_ list
#if MIN_VERSION_base(4,13,0)
>>> :sprint list
list = 1 : _
#else
>>> :sprint list
list = _ : _
#endif
-}
evaluateWHNF_ :: MonadIO m => a -> m ()
evaluateWHNF_ what = (`seq` ()) <$!> evaluateWHNF what
{-# INLINE evaluateWHNF_ #-}
{-# SPECIALIZE evaluateWHNF_ :: a -> IO () #-}

{- | Alias for @evaluateWHNF . force@ with clearer name.

>>> list = [1..5] :: [Int]
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
-- but discards resulting value.

>>> list = [1..5] :: [Int]
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
