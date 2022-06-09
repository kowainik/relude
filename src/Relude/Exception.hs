{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Safe                  #-}
{-# LANGUAGE ViewPatterns          #-}

#if __GLASGOW_HASKELL__ > 802
{-# LANGUAGE DerivingStrategies    #-}
#endif

{- |
Module                  : Relude.Exception
Copyright               : (c) 2016 Stephen Diehl
                          (c) 2016-2018 Serokell
                          (c) 2018-2022 Kowainik
SPDX-License-Identifier : MIT
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Re-exports most useful functionality from the "Control.Exception" module. Also
provides some convenient utilities to throw and handle exceptions.
-}

module Relude.Exception
    ( -- * "Control.Exception" reexports
      Exception (..)
    , SomeException (..)

      -- * 'Bug's
    , Bug (..)
    , bug
    , pattern Exc
    ) where

import Control.Exception (Exception (..), SomeException (..))
import Data.List ((++))
import GHC.Show (Show)
import GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)

import Relude.Function ((.))
import Relude.Monad (Maybe (..))

import qualified Control.Exception as E (displayException, throw, toException)


{- | Type that represents exceptions used in cases when a particular codepath is
not meant to be ever executed, but happens to be executed anyway.
-}
data Bug = Bug SomeException CallStack
#if __GLASGOW_HASKELL__ > 802
    deriving stock (Show)
#else
    deriving (Show)
#endif

instance Exception Bug where
    displayException (Bug e cStack) =
        E.displayException e ++ "\n"
        ++ prettyCallStack cStack

-- | Generate a pure value which, when forced, will throw the given exception
impureThrow :: Exception e => e -> a
impureThrow = E.throw . E.toException

{- | Generate a pure value which, when forced, will synchronously
throw the exception wrapped into 'Bug' data type.
-}
bug :: (HasCallStack, Exception e) => e -> a
bug e = impureThrow (Bug (E.toException e) callStack)

{- | Pattern synonym to easy pattern matching on exceptions. So instead of
writing something like this:

@
isNonCriticalExc :: SomeException -> Bool
isNonCriticalExc e
    | Just (_ :: NodeAttackedError) <- fromException e = True
    | Just DialogUnexpected{} <- fromException e = True
    | otherwise = False
@

you can use 'Exc' pattern synonym:

@
isNonCriticalExc :: SomeException -> Bool
isNonCriticalExc = \case
    Exc (_ :: NodeAttackedError) -> True  -- matching all exceptions of type NodeAttackedError
    Exc DialogUnexpected{} -> True
    _ -> False
@

This pattern is bidirectional. You can use @Exc e@ instead of @toException e@.
-}
pattern Exc :: Exception e => e -> SomeException
pattern Exc e <- (fromException -> Just e)
  where
    Exc e = toException e
