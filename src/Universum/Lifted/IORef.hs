{-# LANGUAGE Safe #-}

-- | Lifted reexports from 'Data.IORef' module.

module Universum.Lifted.IORef
       ( IORef
       , atomicModifyIORef
       , atomicModifyIORef'
       , atomicWriteIORef
       , modifyIORef
       , modifyIORef'
       , newIORef
       , readIORef
       , writeIORef
       ) where

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Function (($), (.))
import Data.IORef (IORef)

import qualified Data.IORef as Ref (atomicModifyIORef, atomicModifyIORef', atomicWriteIORef,
                                    modifyIORef, modifyIORef', newIORef, readIORef, writeIORef)

-- | Lifted version of 'Ref.newIORef'.
newIORef :: MonadIO m => a -> m (IORef a)
newIORef = liftIO . Ref.newIORef
{-# INLINE newIORef #-}

-- | Lifted version of 'Ref.readIORef'.
readIORef :: MonadIO m => IORef a -> m a
readIORef = liftIO . Ref.readIORef
{-# INLINE readIORef #-}

-- | Lifted version of 'Ref.writeIORef'.
writeIORef :: MonadIO m => IORef a -> a -> m ()
writeIORef ref what = liftIO $ Ref.writeIORef ref what
{-# INLINE writeIORef #-}

-- | Lifted version of 'Ref.modifyIORef'.
modifyIORef :: MonadIO m => IORef a -> (a -> a) -> m ()
modifyIORef ref how = liftIO $ Ref.modifyIORef ref how
{-# INLINE modifyIORef #-}

-- | Lifted version of 'Ref.modifyIORef''.
modifyIORef' :: MonadIO m => IORef a -> (a -> a) -> m ()
modifyIORef' ref how = liftIO $ Ref.modifyIORef' ref how
{-# INLINE modifyIORef' #-}

-- | Lifted version of 'Ref.atomicModifyIORef'.
atomicModifyIORef :: MonadIO m => IORef a -> (a -> (a, b)) -> m b
atomicModifyIORef ref how = liftIO $ Ref.atomicModifyIORef ref how
{-# INLINE atomicModifyIORef #-}

-- | Lifted version of 'Ref.atomicModifyIORef''.
atomicModifyIORef' :: MonadIO m => IORef a -> (a -> (a, b)) -> m b
atomicModifyIORef' ref how = liftIO $ Ref.atomicModifyIORef' ref how
{-# INLINE atomicModifyIORef' #-}

-- | Lifted version of 'Ref.atomicWriteIORef'.
atomicWriteIORef :: MonadIO m => IORef a -> a -> m ()
atomicWriteIORef ref what = liftIO $ Ref.atomicWriteIORef ref what
{-# INLINE atomicWriteIORef #-}
