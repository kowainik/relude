{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Safe                  #-}
{-# LANGUAGE ViewPatterns          #-}

{-
Copyright: (c) 2016 Stephen Diehl
           (c) 20016-2018 Serokell
           (c) 2018 Kowainik
License: MIT
-}

-- | Re-exports most useful functionality from 'safe-exceptions'. Also
-- provides some functions to work with exceptions over 'MonadError'.

module Universum.Exception
       ( module Control.Exception
#if ( __GLASGOW_HASKELL__ >= 800 )
       , Bug (..)
       , bug
       , pattern Exc
#endif
       , note
       ) where

import Control.Exception (Exception (..), SomeException (..))

import Control.Monad.Except (MonadError, throwError)
import Universum.Applicative (Applicative (pure))
import Universum.Monad (Maybe (..), maybe)

#if ( __GLASGOW_HASKELL__ >= 800 )
import Data.List ((++))
import GHC.Show (Show)
import GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)

import Universum.Function ((.))

import qualified Control.Exception as E (displayException, throw, toException)

-- | Type that represents exceptions used in cases when a particular codepath
-- is not meant to be ever executed, but happens to be executed anyway.
data Bug = Bug SomeException CallStack
    deriving (Show)

instance Exception Bug where
    displayException (Bug e cStack) = E.displayException e ++ "\n"
                                   ++ prettyCallStack cStack

-- | Generate a pure value which, when forced, will throw the given exception
impureThrow :: Exception e => e -> a
impureThrow = E.throw . E.toException

-- | Generate a pure value which, when forced, will synchronously
-- throw the exception wrapped into 'Bug' data type.
bug :: (HasCallStack, Exception e) => e -> a
bug e = impureThrow (Bug (E.toException e) callStack)
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


#if ( __GLASGOW_HASKELL__ >= 800 )
{- | Pattern synonym to easy pattern matching on exceptions. So intead of
writing something like this:

@
isNonCriticalExc e
    | Just (_ :: NodeAttackedError) <- fromException e = True
    | Just DialogUnexpected{} <- fromException e = True
    | otherwise = False
@

you can use 'Exc' pattern synonym:

@
isNonCriticalExc = \case
    Exc (_ :: NodeAttackedError) -> True  -- matching all exceptions of type 'NodeAttackedError'
    Exc DialogUnexpected{} -> True
    _ -> False
@

This pattern is bidirectional. You can use @Exc e@ instead of @toException e@.
-}
pattern Exc :: Exception e => e -> SomeException
pattern Exc e <- (fromException -> Just e)
  where
    Exc e = toException e
#endif
