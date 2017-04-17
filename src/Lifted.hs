{-# LANGUAGE Safe #-}

-- | Lifted versions of base functions.

module Lifted
       ( module Lifted.Concurrent
       , module Lifted.Env
       , module Lifted.File
       , module Lifted.IORef

       , stToIO
       ) where

import           Lifted.Concurrent
import           Lifted.Env
import           Lifted.File
import           Lifted.IORef

import qualified Control.Monad.ST    as XIO
import           Control.Monad.Trans (MonadIO, liftIO)

-- | Lifted version of 'XIO.stToIO'.
stToIO :: MonadIO m => XIO.ST XIO.RealWorld a -> m a
stToIO a = liftIO (XIO.stToIO a)
{-# INLINE stToIO #-}
