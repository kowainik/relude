{-# LANGUAGE CPP                #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE Safe               #-}

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

appendFile :: MonadIO m => FilePath -> Text -> m ()
appendFile a b = liftIO (XIO.appendFile a b)
{-# INLINABLE appendFile #-}

getContents :: MonadIO m => m Text
getContents = liftIO XIO.getContents
{-# INLINABLE getContents #-}

getLine :: MonadIO m => m Text
getLine = liftIO XIO.getLine
{-# INLINABLE getLine #-}

interact :: MonadIO m => (Text -> Text) -> m ()
interact a = liftIO (XIO.interact a)
{-# INLINABLE interact #-}

readFile :: MonadIO m => FilePath -> m Text
readFile a = liftIO (XIO.readFile a)
{-# INLINABLE readFile #-}

writeFile :: MonadIO m => FilePath -> Text -> m ()
writeFile a b = liftIO (XIO.writeFile a b)
{-# INLINABLE writeFile #-}

----------------------------------------------------------------------------
-- IO
----------------------------------------------------------------------------

getArgs :: MonadIO m => m [String]
getArgs = liftIO (XIO.getArgs)
{-# INLINABLE getArgs #-}

openFile :: MonadIO m => FilePath -> IOMode -> m Handle
openFile a b = liftIO (XIO.openFile a b)
{-# INLINABLE openFile #-}

-- 'withFile' can't be lifted into 'MonadIO', as it uses 'bracket'

exitWith :: MonadIO m => ExitCode -> m a
exitWith a = liftIO (XIO.exitWith a)
{-# INLINABLE exitWith #-}

exitFailure :: MonadIO m => m a
exitFailure = liftIO XIO.exitFailure
{-# INLINABLE exitFailure #-}

exitSuccess :: MonadIO m => m a
exitSuccess = liftIO XIO.exitSuccess
{-# INLINABLE exitSuccess #-}

-- 'die' is available since base-4.8, but it's more convenient to
-- redefine it instead of using CPP
die :: MonadIO m => String -> m ()
die err = liftIO (System.IO.hPutStrLn stderr err) >> exitFailure
{-# INLINABLE die #-}

----------------------------------------------------------------------------
-- ST
----------------------------------------------------------------------------

stToIO :: MonadIO m => ST RealWorld a -> m a
stToIO a = liftIO (XIO.stToIO a)
{-# INLINABLE stToIO #-}
