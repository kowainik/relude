-- | This module contains lifted version of 'XIO.stToIO' function.

module Universum.Lifted.ST
       ( stToIO
       ) where

import Control.Monad.Trans (MonadIO, liftIO)

import qualified Control.Monad.ST as XIO

-- | Lifted version of 'XIO.stToIO'.
stToIO :: MonadIO m => XIO.ST XIO.RealWorld a -> m a
stToIO a = liftIO (XIO.stToIO a)
{-# INLINE stToIO #-}
