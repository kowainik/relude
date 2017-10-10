{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe                  #-}

-- | Re-exports most useful functionality from 'safe-exceptions'. Also
-- provides some functions to work with exceptions over 'MonadError'.

module Exceptions
       ( module Control.Exception.Safe

       , note
       ) where

-- exceptions from safe-exceptions
import           Control.Exception.Safe (Exception, MonadCatch, MonadMask (..),
                                         MonadThrow, SomeException (..), bracket,
                                         bracket_, catch, catchAny, finally, throwM)

import           Control.Applicative    (Applicative (pure))
import           Control.Monad.Except   (MonadError, throwError)
import           Data.Maybe             (Maybe, maybe)

-- To suppress redundant applicative constraint warning on GHC 8.0
-- | Throws error for 'Maybe' if 'Data.Maybe.Nothing' is given.
-- Operates over 'MonadError'.
#if ( __GLASGOW_HASKELL__ >= 800 )
note :: (MonadError e m) => e -> Maybe a -> m a
note err = maybe (throwError err) pure
#else
note :: (MonadError e m, Applicative m) => e -> Maybe a -> m a
note err = maybe (throwError err) pure
#endif
