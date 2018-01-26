{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe                  #-}

-- | Re-exports most useful functionality from 'safe-exceptions'. Also
-- provides some functions to work with exceptions over 'MonadError'.

module Universum.Exception
       ( module Control.Exception.Safe
#if ( __GLASGOW_HASKELL__ >= 800 )
       , Bug (..)
       , bug
#endif
       , note
       ) where

-- exceptions from safe-exceptions
import Control.Exception.Safe (Exception (..), MonadCatch, MonadMask (..), MonadThrow,
                               SomeException (..), bracket, bracketOnError, bracket_, catch,
                               catchAny, displayException, finally, handleAny, mask_, onException,
                               throwM, try, tryAny)

import Control.Applicative (Applicative (pure))
import Control.Monad.Except (MonadError, throwError)
import Data.Maybe (Maybe, maybe)

#if ( __GLASGOW_HASKELL__ >= 800 )
import Data.List ((++))
import GHC.Show (Show)
import GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)

import qualified Control.Exception.Safe as Safe (displayException, impureThrow, toException)

-- | Type that represents exceptions used in cases when a particular codepath
-- is not meant to be ever executed, but happens to be executed anyway.
data Bug = Bug SomeException CallStack
    deriving (Show)

instance Exception Bug where
    displayException (Bug e cStack) = Safe.displayException e ++ "\n"
                                   ++ prettyCallStack cStack

-- | Generate a pure value which, when forced, will synchronously
-- throw the exception wrapped into 'Bug' data type.
bug :: (HasCallStack, Exception e) => e -> a
bug e = Safe.impureThrow (Bug (Safe.toException e) callStack)
#endif

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
