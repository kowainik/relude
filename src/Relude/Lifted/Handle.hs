{-# LANGUAGE Safe #-}

{- |
Copyright:  (c) 2020-2021 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

Lifted functions to work with 'IO' 'Handle's.

@since 1.0.0.0
-}

module Relude.Lifted.Handle
    ( hFlush
    , hIsEOF
    , hSetBuffering
    , hGetBuffering
    , Handle
    , stdin
    , stdout
    , stderr
    , withFile
    , BufferMode (..)
    ) where

import Relude.Base (IO)
import Relude.Function ((.))
import Relude.Bool (Bool (..))
import Relude.Monad.Reexport (MonadIO (..))
import qualified System.IO as IO (Handle, hFlush, hIsEOF, hSetBuffering, hGetBuffering, BufferMode)
import System.IO (Handle, stdin, stdout, stderr, withFile, BufferMode (..))

{- | Lifted version of 'IO.hFlush'.

@since 1.0.0.0
-}
hFlush :: MonadIO m => IO.Handle -> m ()
hFlush = liftIO . IO.hFlush
{-# SPECIALIZE hFlush :: IO.Handle -> IO () #-}
{-# INLINE hFlush #-}

{- | Lifted version of 'IO.hIsEOF'.

@since 1.0.0.0
-}
hIsEOF :: MonadIO m => IO.Handle -> m Bool
hIsEOF = liftIO . IO.hIsEOF
{-# SPECIALIZE hIsEOF :: IO.Handle -> IO Bool #-}
{-# INLINE hIsEOF #-}

{- | Lifted version of 'IO.hSetBuffering'.

@since 1.0.0.0
-}
hSetBuffering :: MonadIO m => IO.Handle -> IO.BufferMode -> m ()
hSetBuffering h = liftIO . IO.hSetBuffering h
{-# SPECIALIZE hSetBuffering :: IO.Handle -> IO.BufferMode -> IO () #-}
{-# INLINE hSetBuffering #-}

{- | Lifted version of 'IO.hGetBuffering'.

@since 1.0.0.0
-}
hGetBuffering :: MonadIO m => IO.Handle -> m IO.BufferMode
hGetBuffering = liftIO . IO.hGetBuffering
{-# SPECIALIZE hGetBuffering :: IO.Handle -> IO IO.BufferMode #-}
{-# INLINE hGetBuffering #-}
