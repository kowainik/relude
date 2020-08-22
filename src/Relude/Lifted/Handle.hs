{- |
Copyright:  (c) 2020 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

Lifted functions to with IO Handles.

@since 0.8.0.0
-}

module Relude.Lifted.Handle
    ( hFlush
    , hIsEOF
    , hSetBuffering
    , hGetBuffering
    , module System.IO
    ) where

import Relude.Base (IO)
import Relude.Function ((.))
import Relude.Bool (Bool (..))
import Relude.Monad.Reexport (MonadIO (..))
import qualified System.IO as IO (Handle, hFlush, hIsEOF, hSetBuffering, hGetBuffering, BufferMode)
import System.IO (Handle, stdin, stdout, stderr, withFile)

{- | Lifted version of 'IO.hFlush'.

@since 0.8.0.0
-}
hFlush :: MonadIO m => IO.Handle -> m ()
hFlush = liftIO . IO.hFlush
{-# SPECIALIZE hFlush :: IO.Handle -> IO () #-}
{-# INLINE hFlush #-}

{- | Lifted version of 'IO.hIsEOF'.

@since 0.8.0.0
-}
hIsEOF :: MonadIO m => IO.Handle -> m Bool
hIsEOF = liftIO . IO.hIsEOF
{-# SPECIALIZE hIsEOF :: IO.Handle -> IO Bool #-}
{-# INLINE hIsEOF #-}

{- | Lifted version of 'IO.hSetBuffering'.

@since 0.8.0.0
-}
hSetBuffering :: MonadIO m => IO.Handle -> IO.BufferMode -> m ()
hSetBuffering = (liftIO .) . IO.hSetBuffering
{-# SPECIALIZE hSetBuffering :: IO.Handle -> IO.BufferMode -> IO () #-}
{-# INLINE hSetBuffering #-}

{- | Lifted version of 'IO.hGetBuffering'.

@since 0.8.0.0
-}
hGetBuffering :: MonadIO m => IO.Handle -> m IO.BufferMode
hGetBuffering = liftIO . IO.hGetBuffering
{-# SPECIALIZE hGetBuffering :: IO.Handle -> IO IO.BufferMode #-}
{-# INLINE hGetBuffering #-}
