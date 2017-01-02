{-# LANGUAGE CPP #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Base (
  module X,
  ($!),
) where

-- Glorious Glasgow Haskell Compiler
#if defined(__GLASGOW_HASKELL__) && ( __GLASGOW_HASKELL__ >= 600 )

-- Base GHC types
import GHC.Num as X (
    Num(..)
  , Integer
  , subtract
  )
import GHC.Enum as X (
    Bounded(..)
  , Enum(..)
  , boundedEnumFrom
  , boundedEnumFromThen
  )
import GHC.Real as X
import GHC.Float as X (
    Float(..)
  , Double(..)
  , showFloat
  , showSignedFloat
  )
import GHC.Err as X (
    undefined
  , error
  )
import GHC.Show as X (
    Show(..)
  )
import GHC.Exts as X (
    Constraint
  , Ptr
  , FunPtr
  )
import GHC.Base as X (
    (++)
  , seq
  , asTypeOf
  , ord
  , maxInt
  , minInt
  )

-- Exported for lifting into new functions.
import System.IO as X (
    print
  , putStr
  , putStrLn
  )

import GHC.Types as X (
    Bool
  , Char
  , Int
  , Word
  , Ordering
  , IO
#if ( __GLASGOW_HASKELL__ >= 710 )
  , Coercible
#endif
  )

#if ( __GLASGOW_HASKELL__ >= 710 )
import GHC.StaticPtr as X (StaticPtr)
#endif

#if ( __GLASGOW_HASKELL__ >= 800 )
import GHC.OverloadedLabels as X (
    IsLabel(..)
  )

import GHC.ExecutionStack as X (
    Location(..)
  , SrcLoc(..)
  , getStackTrace
  , showStackTrace
  )

import GHC.Stack as X (
    CallStack
  , type HasCallStack
  , callStack
  , prettySrcLoc
  , currentCallStack
  , getCallStack
  , prettyCallStack
  , withFrozenCallStack
  )

{-
import GHC.Records as X (
    HasField(..)
  )
-}

#if ( __GLASGOW_HASKELL__ >= 710 )
import GHC.TypeLits as X (
  Symbol,
  SomeSymbol(..),
  Nat,
  SomeNat(..),
  CmpNat,
  KnownSymbol,
  KnownNat,
  natVal,
  someNatVal,
  symbolVal,
  someSymbolVal
  )
#endif

import Data.Kind as X (
    type (*)
  , type Type
  )
#endif

-- Default Prelude defines this at the toplevel module, so we do as well.
infixr 0 $!

($!) :: (a -> b) -> a -> b
f $! x  = let !vx = x in f vx

#endif
