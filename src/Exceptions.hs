{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe                  #-}

module Exceptions
       ( note
       ) where

import           Control.Applicative  (Applicative (pure))
import           Control.Monad.Except (MonadError, throwError)
import           Data.Maybe           (Maybe, maybe)

-- To suppress redundant applicative constraint warning on GHC 8.0
#if ( __GLASGOW_HASKELL__ >= 800 )
note :: (MonadError e m) => e -> Maybe a -> m a
note err = maybe (throwError err) pure
#else
note :: (MonadError e m, Applicative m) => e -> Maybe a -> m a
note err = maybe (throwError err) pure
#endif
