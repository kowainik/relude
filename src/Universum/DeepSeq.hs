-- | This module contains useful functions to evaluate expressions to weak-head
-- normal form or just normal form. Useful to force traces or @error@ inside
-- monadic computation or to remove space leaks.

module Universum.DeepSeq
       ( module Control.DeepSeq
       , evaluateNF
       , evaluateNF_
       , evaluateWHNF
       , evaluateWHNF_
       ) where

import Control.DeepSeq (NFData (..), deepseq, force, ($!!))

import Universum.Base (seq)
import Universum.Function ((.))
import Universum.Monad (MonadIO, liftIO, (<$!>))

import qualified Control.Exception.Base (evaluate)

-- | Lifted alias for 'Control.Exception.Base.evaluate' with clearer name.
evaluateWHNF :: MonadIO m => a -> m a
evaluateWHNF = liftIO . Control.Exception.Base.evaluate

-- | Like 'evaluateWNHF' but discards value.
evaluateWHNF_ :: MonadIO m => a -> m ()
evaluateWHNF_ what = (`seq` ()) <$!> evaluateWHNF what

-- | Alias for @evaluateWHNF . force@ with clearer name.
evaluateNF :: (NFData a, MonadIO m) => a -> m a
evaluateNF = evaluateWHNF . force

-- | Alias for @evaluateWHNF . rnf@. Similar to 'evaluateNF'
-- but discards resulting value.
evaluateNF_ :: (NFData a, MonadIO m) => a -> m ()
evaluateNF_ = evaluateWHNF . rnf
