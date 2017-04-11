{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe                  #-}

-- | Some functions to work with exceptions over 'MonadError'.

module Exceptions
       ( Exception
       , SomeException (..)
       , note
       ) where

import           Control.Applicative  (Applicative (pure))
import           Control.Exception    (Exception, SomeException (..))
import           Control.Monad.Except (MonadError, throwError)
import           Data.Maybe           (Maybe, maybe)

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
