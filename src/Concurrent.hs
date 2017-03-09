{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe              #-}

-- | Concurrency useful and common functions.
module Concurrent
       ( -- * MVar
         MVar
       , newEmptyMVar
       , newMVar
       , putMVar
       , readMVar
       , swapMVar
       , takeMVar
       , tryPutMVar
       , tryReadMVar
       , tryTakeMVar

         -- * STM
       , STM
       , TVar
       , atomically
       , newTVarIO
       , STM.modifyTVar'
       , STM.newTVar
       , STM.readTVar
       , STM.writeTVar
       ) where


import qualified Control.Concurrent.MVar     as CCM (newEmptyMVar, newMVar, putMVar,
                                                     readMVar, swapMVar, takeMVar,
                                                     tryPutMVar, tryReadMVar, tryTakeMVar)
import qualified Control.Concurrent.STM.TVar as STM (modifyTVar', newTVar, newTVarIO,
                                                     readTVar, writeTVar)
import qualified Control.Monad.STM           as STM (atomically)

import           Control.Concurrent.MVar     (MVar)
import           Control.Concurrent.STM.TVar (TVar)
import           Control.Monad.STM           (STM)
import           Control.Monad.Trans         (MonadIO, liftIO)
import           Data.Bool                   (Bool)
import           Data.Function               (($), (.))
import           Data.Maybe                  (Maybe)

----------------------------------------------------------------------------
-- Lifted Control.Concurrent.MVar
----------------------------------------------------------------------------

-- | Lifted to 'MonadIO' version of 'CCM.newEmptyMVar'.
newEmptyMVar :: MonadIO m => m (MVar a)
newEmptyMVar = liftIO CCM.newEmptyMVar
{-# INLINABLE newEmptyMVar #-}

-- | Lifted to 'MonadIO' version of 'CCM.newMVar'.
newMVar :: MonadIO m => a -> m (MVar a)
newMVar = liftIO . CCM.newMVar
{-# INLINABLE newMVar #-}

-- | Lifted to 'MonadIO' version of 'CCM.putMVar'.
putMVar :: MonadIO m => MVar a -> a -> m ()
putMVar m a = liftIO $ CCM.putMVar m a
{-# INLINABLE putMVar #-}

-- | Lifted to 'MonadIO' version of 'CCM.readMVar'.
readMVar :: MonadIO m => MVar a -> m a
readMVar = liftIO . CCM.readMVar
{-# INLINABLE readMVar #-}

-- | Lifted to 'MonadIO' version of 'CCM.swapMVar'.
swapMVar :: MonadIO m => MVar a -> a -> m a
swapMVar m v = liftIO $ CCM.swapMVar m v
{-# INLINABLE swapMVar #-}

-- | Lifted to 'MonadIO' version of 'CCM.takeMVar'.
takeMVar :: MonadIO m => MVar a -> m a
takeMVar = liftIO . CCM.takeMVar
{-# INLINABLE takeMVar #-}

-- | Lifted to 'MonadIO' version of 'CCM.tryPutMVar'.
tryPutMVar :: MonadIO m => MVar a -> a -> m Bool
tryPutMVar m v = liftIO $ CCM.tryPutMVar m v
{-# INLINABLE tryPutMVar #-}

-- | Lifted to 'MonadIO' version of 'CCM.tryReadMVar'.
tryReadMVar :: MonadIO m => MVar a -> m (Maybe a)
tryReadMVar = liftIO . CCM.tryReadMVar
{-# INLINABLE tryReadMVar #-}

-- | Lifted to 'MonadIO' version of 'CCM.tryTakeMVar'.
tryTakeMVar :: MonadIO m => MVar a -> m (Maybe a)
tryTakeMVar = liftIO . CCM.tryTakeMVar
{-# INLINABLE tryTakeMVar #-}

----------------------------------------------------------------------------
-- Lifted STM
----------------------------------------------------------------------------

-- | Lifted to 'MonadIO' version of 'STM.atomically'.
atomically :: MonadIO m => STM a -> m a
atomically = liftIO . STM.atomically
{-# INLINABLE atomically #-}

-- | Lifted to 'MonadIO' version of 'STM.newTVarIO'.
newTVarIO :: MonadIO m => a -> m (TVar a)
newTVarIO = liftIO . STM.newTVarIO
{-# INLINABLE newTVarIO #-}
