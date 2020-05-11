{-# LANGUAGE Safe #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2020 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

Lifted reexports from "Data.IORef" module.
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


-- $setup
-- >>> import Relude

{- | Lifted version of 'Ref.newIORef'.

>>> ref <- newIORef False
>>> :t ref
ref :: IORef Bool

-}
newIORef :: MonadIO m => a -> m (IORef a)
newIORef = liftIO . Ref.newIORef
{-# INLINE newIORef #-}
{-# SPECIALIZE newIORef :: a -> IO (IORef a) #-}

{- | Lifted version of 'Ref.readIORef'.

>>> ref <- newIORef 42
>>> readIORef ref
42

-}
readIORef :: MonadIO m => IORef a -> m a
readIORef = liftIO . Ref.readIORef
{-# INLINE readIORef #-}
{-# SPECIALIZE readIORef :: IORef a -> IO a #-}

{- | Lifted version of 'Ref.writeIORef'.

>>> ref <- newIORef 42
>>> writeIORef ref 43
>>> readIORef ref
43

-}
writeIORef :: MonadIO m => IORef a -> a -> m ()
writeIORef ref what = liftIO $ Ref.writeIORef ref what
{-# INLINE writeIORef #-}
{-# SPECIALIZE writeIORef :: IORef a -> a -> IO () #-}

{- | Lifted version of 'Ref.modifyIORef'.

>>> ref <- newIORef 42
>>> modifyIORef ref (\a -> a + 6)
>>> readIORef ref
48

-}
modifyIORef :: MonadIO m => IORef a -> (a -> a) -> m ()
modifyIORef ref how = liftIO $ Ref.modifyIORef ref how
{-# INLINE modifyIORef #-}
{-# SPECIALIZE modifyIORef :: IORef a -> (a -> a) -> IO () #-}

{- | Lifted version of 'Ref.modifyIORef''.

>>> ref <- newIORef 42
>>> modifyIORef' ref (\a -> a + 3)
>>> readIORef ref
45

-}
modifyIORef' :: MonadIO m => IORef a -> (a -> a) -> m ()
modifyIORef' ref how = liftIO $ Ref.modifyIORef' ref how
{-# INLINE modifyIORef' #-}
{-# SPECIALIZE modifyIORef' :: IORef a -> (a -> a) -> IO () #-}

{- | Lifted version of 'Ref.atomicModifyIORef'.

>>> ref <- newIORef 42
>>> atomicModifyIORef ref (\a -> (a, a + 3))
45
>>> readIORef ref
42

-}
atomicModifyIORef :: MonadIO m => IORef a -> (a -> (a, b)) -> m b
atomicModifyIORef ref how = liftIO $ Ref.atomicModifyIORef ref how
{-# INLINE atomicModifyIORef #-}
{-# SPECIALIZE atomicModifyIORef :: IORef a -> (a -> (a, b)) -> IO b #-}

{- | Lifted version of 'Ref.atomicModifyIORef''.

>>> ref <- newIORef 42
>>> atomicModifyIORef' ref (\a -> (a, a + 3))
45
>>> readIORef ref
42

-}
atomicModifyIORef' :: MonadIO m => IORef a -> (a -> (a, b)) -> m b
atomicModifyIORef' ref how = liftIO $ Ref.atomicModifyIORef' ref how
{-# INLINE atomicModifyIORef' #-}
{-# SPECIALIZE atomicModifyIORef' :: IORef a -> (a -> (a, b)) -> IO b #-}

{- | Lifted version of 'Ref.atomicWriteIORef'.

>>> ref <- newIORef 42
>>> atomicWriteIORef ref 45
>>> readIORef ref
45

-}
atomicWriteIORef :: MonadIO m => IORef a -> a -> m ()
atomicWriteIORef ref what = liftIO $ Ref.atomicWriteIORef ref what
{-# INLINE atomicWriteIORef #-}
{-# SPECIALIZE atomicWriteIORef :: IORef a -> a -> IO () #-}
