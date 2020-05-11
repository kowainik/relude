{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE Trustworthy          #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ > 802
{-# LANGUAGE DerivingStrategies   #-}
#endif

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2020 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

Functions for debugging and prototyping. If you leave these functions in your
code then a warning is generated to remind you about left usages.

@
__ghci>__ foo = trace "I forgot trace in code"

\<interactive\>:4:7: __warning__: [-Wdeprecations]
    In the use of ‘trace’ (imported from "Relude"):
    "'trace' remains in code"
@

__⚠ NOTE:__ Use these functions only for debugging purposes. They break referential
transparency, they are only useful when you want to observe intermediate values
of your pure functions.
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
import GHC.TypeLits (ErrorMessage (..), TypeError)

import Relude.Applicative (Applicative)
import Relude.Base (Bounded, Char, Constraint, Enum, Eq, Generic, HasCallStack, Ord, Show, Type,
                    Typeable)
import Relude.String (Read, String, Text, toString)

import qualified Debug.Trace as Debug
import qualified Prelude


-- $setup
-- >>> import Relude
-- >>> :set -Wno-deprecations

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

>>> increment l = map (+1) l
>>> increment [2, 3, 4]
[3,4,5]

>>> increment l = traceShow l (map (+1) l)
>>> increment [2, 3, 4]
[2,3,4]
[3,4,5]

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
let action :: Maybe Int
    action = do
        x <- Just 3
        traceM ("x: " ++ show x)
        y <- pure 12
        traceM ("y: " ++ show y)
        pure (x*2 + y)
in action
:}
x: 3
y: 12
Just 18
-}
traceM :: (Applicative f) => String -> f ()
traceM = Debug.traceM
{-# WARNING traceM "'traceM' remains in code" #-}

{-|
Like 'traceM', but uses 'Relude.show' on the argument to convert it to a
'String'.

>>> :{
let action :: Maybe Int
    action = do
        x <- Just 3
        traceShowM x
        y <- pure 12
        traceShowM y
        pure (x*2 + y)
in action
:}
3
12
Just 18
-}
traceShowM :: (Show a, Applicative f) => a -> f ()
traceShowM = Debug.traceShowM
{-# WARNING traceShowM "'traceShowM' remains in code" #-}

{- | Version of 'Debug.Trace.traceId' that leaves warning.

>>> traceId "hello"
"hello
hello"
-}
traceId :: String -> String
traceId = Debug.traceId
{-# WARNING traceId "'traceId' remains in code" #-}

----------------------------------------------------------------------------
-- error
----------------------------------------------------------------------------

{- | Throw pure errors. Use this function only to when you are sure that this
branch of code execution is not possible.  __DO NOT USE__ 'error' as a normal
error handling mechanism.

#ifdef mingw32_HOST_OS
>>> error "oops"
*** Exception: oops
CallStack (from HasCallStack):
  error, called at src\\Relude\\Debug.hs:208:11 in ...
  ...
#else
>>> error "oops"
*** Exception: oops
CallStack (from HasCallStack):
  error, called at src/Relude/Debug.hs:208:11 in ...
...
#endif

⚠️__CAUTION__⚠️  Unlike "Prelude" version, 'error' takes 'Relude.Text' as an
argument. In case it used by mistake, the user will see the following:

>>> error ("oops" :: String)
...
... 'error' expects 'Text' but was given 'String'.
      Possible fixes:
          * Make sure OverloadedStrings extension is enabled
          * Use 'error (toText msg)' instead of 'error msg'
...
>>> error False
...
... 'error' works with 'Text'
      But given: Bool
...
-}
error
    :: forall (r :: RuntimeRep) (a :: TYPE r) (t :: Type) .
       (HasCallStack, IsText t)
    => t
    -> a
error e = Prelude.error (toString e)

type IsText (t :: Type) = (t ~ Text, CheckIsText t)

type family CheckIsText (t :: Type) :: Constraint where
    CheckIsText Text = ()
    CheckIsText [Char] = TypeError
        ( 'Text "'error' expects 'Text' but was given 'String'."
        ':$$: 'Text "Possible fixes:"
        ':$$: 'Text "    * Make sure OverloadedStrings extension is enabled"
        ':$$: 'Text "    * Use 'error (toText msg)' instead of 'error msg'"
        )
    CheckIsText a = TypeError
        ( 'Text "'error' works with 'Text'"
        ':$$: 'Text "But given: " ':<>: 'ShowType a
        )

----------------------------------------------------------------------------
-- Undefined and undefined
----------------------------------------------------------------------------

-- | Similar to 'undefined' but data type.
data Undefined = Undefined
#if __GLASGOW_HASKELL__ > 802
    deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Typeable, Generic)
#else
    deriving (Eq, Ord, Show, Read, Enum, Bounded, Data, Typeable, Generic)
#endif
{-# WARNING Undefined "'Undefined' type remains in code" #-}

-- | 'Prelude.undefined' that leaves warning in code on every usage.
undefined :: forall (r :: RuntimeRep) . forall (a :: TYPE r) . HasCallStack => a
undefined = Prelude.undefined
{-# WARNING undefined "'undefined' function remains in code" #-}
