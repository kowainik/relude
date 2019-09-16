{-# LANGUAGE Trustworthy #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Lifted reexports from 'Data.IORef' module.

Lifted meaning that you can also use them inside various
Monad Transformers without adding liftIO call explicitly.
-}

module Relude.Lifted.IORef
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

import Relude.Base (IO)

import qualified Data.IORef as Ref (atomicModifyIORef, atomicModifyIORef', atomicWriteIORef,
                                    modifyIORef, modifyIORef', newIORef, readIORef, writeIORef)


-- | Lifted version of 'Ref.newIORef'.
{-

TODO: newIORef
not too sure how to give examples on these

-}
newIORef :: MonadIO m => a -> m (IORef a)
newIORef = liftIO . Ref.newIORef
{-# INLINE newIORef #-}
{-# SPECIALIZE newIORef :: a -> IO (IORef a) #-}

-- | Lifted version of 'Ref.readIORef'.
readIORef :: MonadIO m => IORef a -> m a
readIORef = liftIO . Ref.readIORef
{-# INLINE readIORef #-}
{-# SPECIALIZE readIORef :: IORef a -> IO a #-}

-- | Lifted version of 'Ref.writeIORef'.
writeIORef :: MonadIO m => IORef a -> a -> m ()
writeIORef ref what = liftIO $ Ref.writeIORef ref what
{-# INLINE writeIORef #-}
{-# SPECIALIZE writeIORef :: IORef a -> a -> IO () #-}

-- | Lifted version of 'Ref.modifyIORef'.
modifyIORef :: MonadIO m => IORef a -> (a -> a) -> m ()
modifyIORef ref how = liftIO $ Ref.modifyIORef ref how
{-# INLINE modifyIORef #-}
{-# SPECIALIZE modifyIORef :: IORef a -> (a -> a) -> IO () #-}

-- | Lifted version of 'Ref.modifyIORef''.
modifyIORef' :: MonadIO m => IORef a -> (a -> a) -> m ()
modifyIORef' ref how = liftIO $ Ref.modifyIORef' ref how
{-# INLINE modifyIORef' #-}
{-# SPECIALIZE modifyIORef' :: IORef a -> (a -> a) -> IO () #-}

-- | Lifted version of 'Ref.atomicModifyIORef'.
atomicModifyIORef :: MonadIO m => IORef a -> (a -> (a, b)) -> m b
atomicModifyIORef ref how = liftIO $ Ref.atomicModifyIORef ref how
{-# INLINE atomicModifyIORef #-}
{-# SPECIALIZE atomicModifyIORef :: IORef a -> (a -> (a, b)) -> IO b #-}

-- | Lifted version of 'Ref.atomicModifyIORef''.
atomicModifyIORef' :: MonadIO m => IORef a -> (a -> (a, b)) -> m b
atomicModifyIORef' ref how = liftIO $ Ref.atomicModifyIORef' ref how
{-# INLINE atomicModifyIORef' #-}
{-# SPECIALIZE atomicModifyIORef' :: IORef a -> (a -> (a, b)) -> IO b #-}

-- | Lifted version of 'Ref.atomicWriteIORef'.
atomicWriteIORef :: MonadIO m => IORef a -> a -> m ()
atomicWriteIORef ref what = liftIO $ Ref.atomicWriteIORef ref what
{-# INLINE atomicWriteIORef #-}
{-# SPECIALIZE atomicWriteIORef :: IORef a -> a -> IO () #-}
