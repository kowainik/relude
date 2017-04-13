{-# LANGUAGE Safe #-}

-- | Lifted versions of functions that work with environment.

module Lifted.Env
       ( getArgs
       , exitWith
       , exitFailure
       , exitSuccess
       , die
       ) where

import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.String         (String)
import           Prelude             ((>>))
import qualified System.Environment  as XIO
import           System.Exit         (ExitCode)
import qualified System.Exit         as XIO
import           System.IO           (stderr)
import qualified System.IO           (hPutStrLn)

-- | Lifted version of 'System.Environment.getArgs'.
getArgs :: MonadIO m => m [String]
getArgs = liftIO (XIO.getArgs)
{-# INLINE getArgs #-}

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
-- 'XIO.die' is available since base-4.8, but it's more convenient to
-- redefine it instead of using CPP.
die :: MonadIO m => String -> m ()
die err = liftIO (System.IO.hPutStrLn stderr err) >> exitFailure
{-# INLINE die #-}
