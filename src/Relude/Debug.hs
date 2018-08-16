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
Copyright: (c) 2016 Stephen Diehl
           (c) 20016-2018 Serokell
           (c) 2018 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Functions for debugging. If you left these functions in your code then a warning
is generated to remind you about left usages. Also some functions (and data
types) are convenient for prototyping.
-}

module Relude.Debug
       ( Undefined (..)
       , error
       , trace
       , traceM
       , traceId
       , traceShow
       , traceShowId
       , traceShowM
       , undefined
       ) where

import Data.Data (Data)
import GHC.Exts (RuntimeRep, TYPE)
import System.IO.Unsafe (unsafePerformIO)

import Relude.Applicative (pass)
import Relude.Base (Generic, HasCallStack, Typeable)
import Relude.Monad.Reexport (Monad (..))
import Relude.Print (Print, putStrLn)
import Relude.String (Text, toString)

import qualified Prelude as P

-- | Generalized over string version of 'Debug.Trace.trace' that leaves warnings.
{-# WARNING trace "'trace' remains in code" #-}
trace :: Print b => b -> a -> a
trace string expr = unsafePerformIO (do
    putStrLn string
    return expr)

-- | 'P.error' that takes 'Text' as an argument.
error :: forall (r :: RuntimeRep) . forall (a :: TYPE r) . HasCallStack
      => Text -> a
error s = P.error (toString s)

-- | Version of 'Debug.Trace.traceShow' that leaves warning.
{-# WARNING traceShow "'traceShow' remains in code" #-}
traceShow :: P.Show a => a -> b -> b
traceShow a b = trace (P.show a) b

-- | Version of 'Debug.Trace.traceShow' that leaves warning.
{-# WARNING traceShowId "'traceShowId' remains in code" #-}
traceShowId :: P.Show a => a -> a
traceShowId a = trace (P.show a) a

-- | Version of 'Debug.Trace.traceShowM' that leaves warning.
{-# WARNING traceShowM "'traceShowM' remains in code" #-}
traceShowM :: (P.Show a, Monad m) => a -> m ()
traceShowM a = trace (P.show a) pass

-- | Version of 'Debug.Trace.traceM' that leaves warning and takes 'Text'.
{-# WARNING traceM "'traceM' remains in code" #-}
traceM :: (Monad m) => Text -> m ()
traceM s = trace (toString s) pass

-- | Version of 'Debug.Trace.traceId' that leaves warning and takes 'Text'.
{-# WARNING traceId "'traceId' remains in code" #-}
traceId :: Text -> Text
traceId s = trace s s

-- | Similar to 'undefined' but data type.
{-# WARNING Undefined "'Undefined' type remains in code" #-}
data Undefined = Undefined
    deriving (P.Eq, P.Ord, P.Show, P.Read, P.Enum, P.Bounded, Data, Typeable, Generic)

-- | 'P.undefined' that leaves warning in code on every usage.
{-# WARNING undefined "'undefined' function remains in code (or use 'error')" #-}
undefined :: forall (r :: RuntimeRep) . forall (a :: TYPE r) . HasCallStack => a
undefined = P.undefined
