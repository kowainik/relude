{-# LANGUAGE Safe #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 20016-2018 Serokell
            (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Lifted 'MVar' and 'STM' functions.
-}

module Relude.Lifted.Concurrent
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
       , readTVarIO
       , STM.modifyTVar'
       , STM.newTVar
       , STM.readTVar
       , STM.writeTVar
       ) where

import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.STM (STM)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Bool (Bool)
import Data.Function (($), (.))
import Data.Maybe (Maybe)

import qualified Control.Concurrent.MVar as CCM (newEmptyMVar, newMVar, putMVar, readMVar, swapMVar,
                                                 takeMVar, tryPutMVar, tryReadMVar, tryTakeMVar)
import qualified Control.Concurrent.STM.TVar as STM (modifyTVar', newTVar, newTVarIO, readTVar,
                                                     readTVarIO, writeTVar)
import qualified Control.Monad.STM as STM (atomically)

----------------------------------------------------------------------------
-- Lifted Control.Concurrent.MVar
----------------------------------------------------------------------------

-- | Lifted to 'MonadIO' version of 'CCM.newEmptyMVar'.
newEmptyMVar :: MonadIO m => m (MVar a)
newEmptyMVar = liftIO CCM.newEmptyMVar
{-# INLINE newEmptyMVar #-}

-- | Lifted to 'MonadIO' version of 'CCM.newMVar'.
newMVar :: MonadIO m => a -> m (MVar a)
newMVar = liftIO . CCM.newMVar
{-# INLINE newMVar #-}

-- | Lifted to 'MonadIO' version of 'CCM.putMVar'.
putMVar :: MonadIO m => MVar a -> a -> m ()
putMVar m a = liftIO $ CCM.putMVar m a
{-# INLINE putMVar #-}

-- | Lifted to 'MonadIO' version of 'CCM.readMVar'.
readMVar :: MonadIO m => MVar a -> m a
readMVar = liftIO . CCM.readMVar
{-# INLINE readMVar #-}

-- | Lifted to 'MonadIO' version of 'CCM.swapMVar'.
swapMVar :: MonadIO m => MVar a -> a -> m a
swapMVar m v = liftIO $ CCM.swapMVar m v
{-# INLINE swapMVar #-}

-- | Lifted to 'MonadIO' version of 'CCM.takeMVar'.
takeMVar :: MonadIO m => MVar a -> m a
takeMVar = liftIO . CCM.takeMVar
{-# INLINE takeMVar #-}

-- | Lifted to 'MonadIO' version of 'CCM.tryPutMVar'.
tryPutMVar :: MonadIO m => MVar a -> a -> m Bool
tryPutMVar m v = liftIO $ CCM.tryPutMVar m v
{-# INLINE tryPutMVar #-}

-- | Lifted to 'MonadIO' version of 'CCM.tryReadMVar'.
tryReadMVar :: MonadIO m => MVar a -> m (Maybe a)
tryReadMVar = liftIO . CCM.tryReadMVar
{-# INLINE tryReadMVar #-}

-- | Lifted to 'MonadIO' version of 'CCM.tryTakeMVar'.
tryTakeMVar :: MonadIO m => MVar a -> m (Maybe a)
tryTakeMVar = liftIO . CCM.tryTakeMVar
{-# INLINE tryTakeMVar #-}

----------------------------------------------------------------------------
-- Lifted STM
----------------------------------------------------------------------------

-- | Lifted to 'MonadIO' version of 'STM.atomically'.
atomically :: MonadIO m => STM a -> m a
atomically = liftIO . STM.atomically
{-# INLINE atomically #-}

-- | Lifted to 'MonadIO' version of 'STM.newTVarIO'.
newTVarIO :: MonadIO m => a -> m (TVar a)
newTVarIO = liftIO . STM.newTVarIO
{-# INLINE newTVarIO #-}

-- | Lifted to 'MonadIO' version of 'STM.readTVarIO'.
readTVarIO :: MonadIO m => TVar a -> m a
readTVarIO = liftIO . STM.readTVarIO
{-# INLINE readTVarIO #-}
