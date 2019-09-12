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

Use these functions only for debugging purposes. They break referential trasparency,
they are only useful when you want to observe intermediate values of your pure functions.
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

{- | Version of 'Debug.Trace.trace' that leaves warning.

>>> increment l = map (+1) l
>>> increment [2, 3, 4]
[3,4,5]

>>> increment l = trace ("incrementing each value of: " ++ show l) (map (+1) l)
>>> increment [2, 3, 4]
incrementing each value of: [2,3,4]
[3,4,5]

-}
trace :: String -> a -> a
trace = Debug.trace
{-# WARNING trace "'trace' remains in code" #-}

{- | Version of 'Debug.Trace.traceShow' that leaves warning.

>>> let x = 123
>>> let f = show
>>> trace x (f (x + x))
123
246

-}
traceShow :: Show a => a -> b -> b
traceShow = Debug.traceShow
{-# WARNING traceShow "'traceShow' remains in code" #-}

{- | Version of 'Debug.Trace.traceShowId' that leaves warning.

>>> traceShowId (1+2+3, "hello" ++ "world")
(6,"helloworld")
(6,"helloworld")

-}
traceShowId :: Show a => a -> a
traceShowId = Debug.traceShowId
{-# WARNING traceShowId "'traceShowId' remains in code" #-}

{- | Version of 'Debug.Trace.traceM' that leaves warning.

>>> :{
  do
    x <- Just 3
    traceM ("x: " ++ show x)
    y <- pure 12
    traceM ("y: " ++ show y)
    pure (x*2 + y)
:}

> x: 3
> y: 12
> Just 18

-}
traceM :: (Applicative f) => String -> f ()
traceM = Debug.traceM
{-# WARNING traceM "'traceM' remains in code" #-}

{-|
Like 'traceM', but uses 'show' on the argument to convert it to a 'String'.

@
  do
    x <- Just 3
    traceShowM x
    y <- pure 12
    traceShowM y
    pure (x*2 + y)
@

> 3
> 12
> Just 18

-}
traceShowM :: (Show a, Applicative f) => a -> f ()
traceShowM = Debug.traceShowM
{-# WARNING traceShowM "'traceShowM' remains in code" #-}

{- | Version of 'Debug.Trace.traceId' that leaves warning.

>>> traceId "hello"
hello
hello

-}
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
