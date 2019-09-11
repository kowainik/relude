{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE Trustworthy        #-}

#if ( __GLASGOW_HASKELL__ >= 804 )
{-# LANGUAGE TypeInType         #-}
#endif

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Functions for debugging. If you left these functions in your code then a warning
is generated to remind you about left usages. Also some functions (and data
types) are convenient for prototyping.
-}

module Relude.Debug
       ( -- * Tracing
         trace
       , traceM
       , traceId
       , traceShow
       , traceShowId
       , traceShowM

         -- * Imprecise error
       , error
       , Undefined (..)
       , undefined
       ) where

import Data.Data (Data)
import GHC.Exts (RuntimeRep, TYPE)

import Relude.Applicative (Applicative)
import Relude.Base (Bounded, Enum, Eq, Generic, HasCallStack, Ord, Show, Typeable)
import Relude.String (Read, String, Text, toString)

import qualified Debug.Trace as Debug
import qualified Prelude

----------------------------------------------------------------------------
-- trace
----------------------------------------------------------------------------

-- | Version of 'Debug.Trace.trace' that leaves warning.
trace :: String -> a -> a
trace = Debug.trace
{-# WARNING trace "'trace' remains in code" #-}

-- | Version of 'Debug.Trace.traceShow' that leaves warning.
traceShow :: Show a => a -> b -> b
traceShow = Debug.traceShow
{-# WARNING traceShow "'traceShow' remains in code" #-}

-- | Version of 'Debug.Trace.traceShowId' that leaves warning.
traceShowId :: Show a => a -> a
traceShowId = Debug.traceShowId
{-# WARNING traceShowId "'traceShowId' remains in code" #-}

-- | Version of 'Debug.Trace.traceShowM' that leaves warning.
traceShowM :: (Show a, Applicative f) => a -> f ()
traceShowM = Debug.traceShowM
{-# WARNING traceShowM "'traceShowM' remains in code" #-}

-- | Version of 'Debug.Trace.traceM' that leaves warning.
traceM :: (Applicative f) => String -> f ()
traceM = Debug.traceM
{-# WARNING traceM "'traceM' remains in code" #-}

-- | Version of 'Debug.Trace.traceId' that leaves warning.
traceId :: String -> String
traceId = Debug.traceId
{-# WARNING traceId "'traceId' remains in code" #-}

----------------------------------------------------------------------------
-- error and undefined
----------------------------------------------------------------------------

-- | 'Prelude.error' that takes 'Text' as an argument.
error :: forall (r :: RuntimeRep) . forall (a :: TYPE r) . HasCallStack
      => Text -> a
error e = Prelude.error (toString e)

-- | Similar to 'undefined' but data type.
data Undefined = Undefined
    deriving (Eq, Ord, Show, Read, Enum, Bounded, Data, Typeable, Generic)
{-# WARNING Undefined "'Undefined' type remains in code" #-}

-- | 'Prelude.undefined' that leaves warning in code on every usage.
undefined :: forall (r :: RuntimeRep) . forall (a :: TYPE r) . HasCallStack => a
undefined = Prelude.undefined
{-# WARNING undefined "'undefined' function remains in code" #-}
