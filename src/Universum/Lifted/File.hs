{-# LANGUAGE Safe #-}

{-
Copyright: (c) 2016 Stephen Diehl
           (c) 20016-2018 Serokell
           (c) 2018 Kowainik
License: MIT
-}

-- | Lifted versions of functions working with files and common IO.
-- All functions are specialized to 'Data.Text.Text'.

module Universum.Lifted.File
       ( appendFile
       , getLine
       , openFile
       , readFile
       , writeFile
       ) where

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Text (Text)
import Prelude (FilePath)
import System.IO (Handle, IOMode)

import qualified Data.Text.IO as XIO
import qualified System.IO as XIO (openFile)

----------------------------------------------------------------------------
-- Text
----------------------------------------------------------------------------

-- | Lifted version of 'Data.Text.appendFile'.
appendFile :: MonadIO m => FilePath -> Text -> m ()
appendFile a b = liftIO (XIO.appendFile a b)
{-# INLINE appendFile #-}

-- | Lifted version of 'Data.Text.getLine'.
getLine :: MonadIO m => m Text
getLine = liftIO XIO.getLine
{-# INLINE getLine #-}

-- | Lifted version of 'Data.Text.readFile'.
readFile :: MonadIO m => FilePath -> m Text
readFile a = liftIO (XIO.readFile a)
{-# INLINE readFile #-}

-- | Lifted version of 'Data.Text.writeFile'.
writeFile :: MonadIO m => FilePath -> Text -> m ()
writeFile a b = liftIO (XIO.writeFile a b)
{-# INLINE writeFile #-}

-- | Lifted version of 'System.IO.openFile'.
openFile :: MonadIO m => FilePath -> IOMode -> m Handle
openFile a b = liftIO (XIO.openFile a b)
{-# INLINE openFile #-}

-- 'withFile' can't be lifted into 'MonadIO', as it uses 'bracket'
