{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE Trustworthy        #-}

-- | Functions for debugging. If you left these functions in your code
-- then warning is generated to remind you about left usages. Also some
-- functions (and data types) are convenient for prototyping.

module Universum.Debug
       ( undefined
       , error
       , trace
       , traceM
       , traceId
       , traceShow
       , traceShowId
       , traceShowM
       , Undefined (..)
       ) where

import Control.Monad (Monad, return)
import Data.Data (Data)
import Data.Text (Text, unpack)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)

import Universum.Applicative (pass)
import Universum.Print (Print, putStrLn)

import qualified Prelude as P

-- | Generalized over string version of 'Debug.Trace.trace' that leaves warnings.
{-# WARNING trace "'trace' remains in code" #-}
trace :: Print b => b -> a -> a
trace string expr = unsafePerformIO (do
    putStrLn string
    return expr)

-- | 'P.error' that takes 'Text' as an argument.
error :: Text -> a
error s = P.error (unpack s)

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
traceM s = trace (unpack s) pass

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
undefined :: a
undefined = P.undefined
