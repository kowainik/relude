{-# LANGUAGE Trustworthy #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
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

import Relude.Base (IO)
import Relude.Bool (Bool)
import Relude.Function (($), (.))
import Relude.Monad (Maybe, MonadIO (..))

import qualified Control.Concurrent.MVar as CCM (newEmptyMVar, newMVar, putMVar, readMVar, swapMVar,
                                                 takeMVar, tryPutMVar, tryReadMVar, tryTakeMVar)
import qualified Control.Concurrent.STM.TVar as STM (modifyTVar', newTVar, newTVarIO, readTVar,
                                                     readTVarIO, writeTVar)
import qualified Control.Monad.STM as STM (atomically)

----------------------------------------------------------------------------
-- Lifted Control.Concurrent.MVar
----------------------------------------------------------------------------

{- | Lifted to 'MonadIO' version of 'CCM.newEmptyMVar'.

TODO: newEmptyMVar
not sure if should create a top level doc or give examples for each.
Just the original docs are descriptive but don't give code examples.

-}
newEmptyMVar :: MonadIO m => m (MVar a)
newEmptyMVar = liftIO CCM.newEmptyMVar
{-# INLINE newEmptyMVar #-}
{-# SPECIALIZE newEmptyMVar :: IO (MVar a) #-}

-- | Lifted to 'MonadIO' version of 'CCM.newMVar'.
newMVar :: MonadIO m => a -> m (MVar a)
newMVar = liftIO . CCM.newMVar
{-# INLINE newMVar #-}
{-# SPECIALIZE newMVar :: a -> IO (MVar a) #-}

-- | Lifted to 'MonadIO' version of 'CCM.putMVar'.
putMVar :: MonadIO m => MVar a -> a -> m ()
putMVar m a = liftIO $ CCM.putMVar m a
{-# INLINE putMVar #-}
{-# SPECIALIZE putMVar :: MVar a -> a -> IO () #-}

-- | Lifted to 'MonadIO' version of 'CCM.readMVar'.
readMVar :: MonadIO m => MVar a -> m a
readMVar = liftIO . CCM.readMVar
{-# INLINE readMVar #-}
{-# SPECIALIZE readMVar :: MVar a -> IO a #-}

-- | Lifted to 'MonadIO' version of 'CCM.swapMVar'.
swapMVar :: MonadIO m => MVar a -> a -> m a
swapMVar m v = liftIO $ CCM.swapMVar m v
{-# INLINE swapMVar #-}
{-# SPECIALIZE swapMVar :: MVar a -> a -> IO a #-}

-- | Lifted to 'MonadIO' version of 'CCM.takeMVar'.
takeMVar :: MonadIO m => MVar a -> m a
takeMVar = liftIO . CCM.takeMVar
{-# INLINE takeMVar #-}
{-# SPECIALIZE takeMVar :: MVar a -> IO a #-}

-- | Lifted to 'MonadIO' version of 'CCM.tryPutMVar'.
tryPutMVar :: MonadIO m => MVar a -> a -> m Bool
tryPutMVar m v = liftIO $ CCM.tryPutMVar m v
{-# INLINE tryPutMVar #-}
{-# SPECIALIZE tryPutMVar :: MVar a -> a -> IO Bool #-}

-- | Lifted to 'MonadIO' version of 'CCM.tryReadMVar'.
tryReadMVar :: MonadIO m => MVar a -> m (Maybe a)
tryReadMVar = liftIO . CCM.tryReadMVar
{-# INLINE tryReadMVar #-}
{-# SPECIALIZE tryReadMVar :: MVar a -> IO (Maybe a) #-}

-- | Lifted to 'MonadIO' version of 'CCM.tryTakeMVar'.
tryTakeMVar :: MonadIO m => MVar a -> m (Maybe a)
tryTakeMVar = liftIO . CCM.tryTakeMVar
{-# INLINE tryTakeMVar #-}
{-# SPECIALIZE tryTakeMVar :: MVar a -> IO (Maybe a) #-}

----------------------------------------------------------------------------
-- Lifted STM
----------------------------------------------------------------------------

-- | Lifted to 'MonadIO' version of 'STM.atomically'.
atomically :: MonadIO m => STM a -> m a
atomically = liftIO . STM.atomically
{-# INLINE atomically #-}
{-# SPECIALIZE atomically :: STM a -> IO a #-}

-- | Lifted to 'MonadIO' version of 'STM.newTVarIO'.
newTVarIO :: MonadIO m => a -> m (TVar a)
newTVarIO = liftIO . STM.newTVarIO
{-# INLINE newTVarIO #-}
{-# SPECIALIZE newTVarIO :: a -> IO (TVar a) #-}

-- | Lifted to 'MonadIO' version of 'STM.readTVarIO'.
readTVarIO :: MonadIO m => TVar a -> m a
readTVarIO = liftIO . STM.readTVarIO
{-# INLINE readTVarIO #-}
{-# SPECIALIZE readTVarIO :: TVar a -> IO a #-}
