{-# LANGUAGE CPP  #-}
{-# LANGUAGE Safe #-}

-- | Lifted versions of functions working with files and environment.
-- All functions are specialized to 'Data.Text.Text` if possible.

module Lifted
       ( -- * Text
         appendFile
       , getContents
       , getLine
       , interact
       , readFile
       , writeFile

         -- * IO
       , getArgs
       , openFile
       , exitWith
       , exitFailure
       , exitSuccess
       , die

         -- * ST
       , stToIO
       ) where

#if ( __GLASGOW_HASKELL__ >= 710 )
import           Control.Monad.ST      (RealWorld, ST)
#else
import           Control.Monad.ST.Safe (RealWorld, ST)
#endif
import           Control.Monad.Trans   (MonadIO, liftIO)
import           Data.String           (String)
import           Data.Text             (Text)
import           Prelude               (FilePath, (>>))
import           System.Exit           (ExitCode)
import           System.IO             (Handle, IOMode, stderr)
import qualified System.IO             (hPutStrLn)

-- Text
import qualified Data.Text.IO          as XIO
-- IO
import qualified System.Environment    as XIO
import qualified System.Exit           as XIO
import qualified System.IO             as XIO (openFile)
-- ST
#if ( __GLASGOW_HASKELL__ >= 710 )
import qualified Control.Monad.ST      as XIO
#else
import qualified Control.Monad.ST.Safe as XIO
#endif

----------------------------------------------------------------------------
-- Text
----------------------------------------------------------------------------

-- | Lifted version of 'Data.Text.appendFile'.
appendFile :: MonadIO m => FilePath -> Text -> m ()
appendFile a b = liftIO (XIO.appendFile a b)
{-# INLINE appendFile #-}

-- | Lifted version of 'Data.Text.getContents'.
getContents :: MonadIO m => m Text
getContents = liftIO XIO.getContents
{-# INLINE getContents #-}

-- | Lifted version of 'Data.Text.getLine'.
getLine :: MonadIO m => m Text
getLine = liftIO XIO.getLine
{-# INLINE getLine #-}

-- | Lifted version of 'Data.Text.interact'.
interact :: MonadIO m => (Text -> Text) -> m ()
interact a = liftIO (XIO.interact a)
{-# INLINE interact #-}

-- | Lifted version of 'Data.Text.readFile'.
readFile :: MonadIO m => FilePath -> m Text
readFile a = liftIO (XIO.readFile a)
{-# INLINE readFile #-}

-- | Lifted version of 'Data.Text.writeFile'.
writeFile :: MonadIO m => FilePath -> Text -> m ()
writeFile a b = liftIO (XIO.writeFile a b)
{-# INLINE writeFile #-}

----------------------------------------------------------------------------
-- IO
----------------------------------------------------------------------------

-- | Lifted version of 'System.Environment.getArgs'.
getArgs :: MonadIO m => m [String]
getArgs = liftIO (XIO.getArgs)
{-# INLINE getArgs #-}

-- | Lifted version of 'System.IO.openFile'.
openFile :: MonadIO m => FilePath -> IOMode -> m Handle
openFile a b = liftIO (XIO.openFile a b)
{-# INLINE openFile #-}

-- 'withFile' can't be lifted into 'MonadIO', as it uses 'bracket'

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

----------------------------------------------------------------------------
-- ST
----------------------------------------------------------------------------

-- | Lifted version of 'XIO.stToIO'.
stToIO :: MonadIO m => ST RealWorld a -> m a
stToIO a = liftIO (XIO.stToIO a)
{-# INLINE stToIO #-}
