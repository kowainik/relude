{- |
Copyright: (c) 2016 Stephen Diehl
           (c) 20016-2018 Serokell
           (c) 2018 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Lifted versions of functions working with files and common IO.
-}

module Relude.Lifted.File
       ( readFile
       , writeFile
       , appendFile
       , openFile
       , hClose
       ) where

import Relude.Base (FilePath, Handle, IO, IOMode, String)
import Relude.Function ((.))
import Relude.Monad.Reexport (MonadIO (..))

import qualified System.IO as IO


-- | Lifted version of 'IO.readFile'.
readFile :: MonadIO m => FilePath -> m String
readFile = liftIO . IO.readFile
{-# SPECIALIZE readFile :: FilePath -> IO String #-}
{-# INLINE readFile #-}

-- | Lifted version of 'IO.writeFile'.
writeFile :: MonadIO m => FilePath -> String -> m ()
writeFile p= liftIO . IO.writeFile p
{-# SPECIALIZE writeFile :: FilePath -> String -> IO () #-}
{-# INLINE writeFile #-}

-- | Lifted version of 'IO.appendFile'.
appendFile :: MonadIO m => FilePath -> String -> m ()
appendFile p = liftIO . IO.appendFile p
{-# SPECIALIZE appendFile :: FilePath -> String -> IO () #-}
{-# INLINE appendFile #-}

-- | Lifted version of 'IO.openFile'.
openFile :: MonadIO m => FilePath -> IOMode -> m Handle
openFile p = liftIO . IO.openFile p
{-# SPECIALIZE openFile :: FilePath -> IOMode -> IO Handle #-}
{-# INLINE openFile #-}

-- | Lifted version of 'IO.hClose'.
hClose :: MonadIO m => Handle -> m ()
hClose = liftIO . hClose
{-# SPECIALIZE hClose :: Handle -> IO () #-}
{-# INLINE hClose #-}
