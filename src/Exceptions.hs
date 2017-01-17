{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy           #-}

module Exceptions
       ( hush
       , note
       , tryIO
       ) where

import           Base                   (IO)
import           Control.Applicative
import           Control.Exception      as Exception
import           Control.Monad.Except   (ExceptT (..), MonadError, throwError)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans    (liftIO)
import           Data.Either            (Either (..))
import           Data.Function          ((.))
import           Data.Maybe             (Maybe, maybe)

hush :: Alternative m => Either e a -> m a
hush (Left _)  = empty
hush (Right x) = pure x

-- To suppress redundenet applicative constraint warning on GHC 8.0
#if ( __GLASGOW_HASKELL__ >= 800 )
note :: (MonadError e m) => e -> Maybe a -> m a
note err = maybe (throwError err) pure
#else
note :: (MonadError e m, Applicative m) => e -> Maybe a -> m a
note err = maybe (throwError err) pure
#endif

tryIO :: MonadIO m => IO a -> ExceptT IOException m a
tryIO = ExceptT . liftIO . Exception.try
