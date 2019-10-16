{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2019 Kowainik
SPDX-License-Identifier: MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Lifted versions of functions working with files and common IO.
-}

module Relude.Lifted.File
       ( readFile
       , writeFile
       , appendFile
       ) where

import Relude.Base (FilePath, IO)
import Relude.Function ((.))
import Relude.Monad.Reexport (MonadIO (..))
import Relude.String (String)

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
