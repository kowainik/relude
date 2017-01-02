{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Exceptions (
  hush,
  note,
  tryIO,
) where

import Base (IO)
import Data.Function ((.))
import Control.Monad.Trans (liftIO)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (ExceptT(..), MonadError, throwError)
import Control.Exception as Exception
import Control.Applicative
import Data.Maybe (Maybe, maybe)
import Data.Either (Either(..))

hush :: Alternative m => Either e a -> m a
hush (Left _)  = empty
hush (Right x) = pure x

#if ( __GLASGOW_HASKELL__ >= 800 )
note :: (MonadError e m) => e -> Maybe a -> m a
note err = maybe (throwError err) pure
#else
note :: (MonadError e m, Applicative m) => e -> Maybe a -> m a
note err = maybe (throwError err) pure
#endif

tryIO :: MonadIO m => IO a -> ExceptT IOException m a
tryIO = ExceptT . liftIO . Exception.try
