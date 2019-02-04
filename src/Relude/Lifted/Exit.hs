{-# LANGUAGE Safe #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Lifted versions of functions that work with exit processes.
-}

module Relude.Lifted.Exit
       ( exitWith
       , exitFailure
       , exitSuccess
       , die
       ) where

import Control.Monad.Trans (MonadIO, liftIO)
import Data.String (String)
import System.Exit (ExitCode)

import qualified System.Exit as XIO

-- | Lifted version of 'System.Exit.exitWith'.
exitWith :: MonadIO m => ExitCode -> m a
exitWith a = liftIO (XIO.exitWith a)
{-# INLINE exitWith #-}

-- | Lifted version of 'System.Exit.exitFailure'.
exitFailure :: MonadIO m => m a
exitFailure = liftIO XIO.exitFailure
{-# INLINE exitFailure #-}

-- | Lifted version of 'System.Exit.exitSuccess'.
exitSuccess :: MonadIO m => m a
exitSuccess = liftIO XIO.exitSuccess
{-# INLINE exitSuccess #-}

-- | Lifted version of 'System.Exit.die'.
die :: MonadIO m => String -> m ()
die err = liftIO (XIO.die err)
{-# INLINE die #-}
