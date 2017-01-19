{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE Unsafe             #-}

module Base
       ( module X
       , ($!)
       ) where

-- Glorious Glasgow Haskell Compiler
#if defined(__GLASGOW_HASKELL__) && ( __GLASGOW_HASKELL__ >= 600 )

-- Base GHC types
import           GHC.Base             as X (asTypeOf, maxInt, minInt, ord, seq, (++))
import           GHC.Enum             as X (Bounded (..), Enum (..), boundedEnumFrom,
                                            boundedEnumFromThen)
import           GHC.Err              as X (error, undefined)
import           GHC.Exts             as X (Constraint, FunPtr, Ptr)
import           GHC.Float            as X (Double (..), Float (..), Floating (..),
                                            showFloat, showSignedFloat)
import           GHC.Num              as X (Integer, Num (..), subtract)
import           GHC.Real             as X hiding ((%))
import           GHC.Show             as X (Show (..))
import           System.IO            as X (print, putStr, putStrLn)

import           GHC.Types            as X (Bool, Char, IO, Int, Ordering, Word)


#if ( __GLASGOW_HASKELL__ >= 710 )
import           GHC.Types            as X (Coercible)
#endif

#if ( __GLASGOW_HASKELL__ >= 710 )
import           GHC.StaticPtr        as X (StaticPtr)
#endif

#if ( __GLASGOW_HASKELL__ >= 800 )
import           GHC.OverloadedLabels as X (IsLabel (..))

import           GHC.ExecutionStack   as X (Location (..), SrcLoc (..), getStackTrace,
                                            showStackTrace)

import           GHC.Stack            as X (CallStack, HasCallStack, callStack,
                                            currentCallStack, getCallStack,
                                            prettyCallStack, prettySrcLoc,
                                            withFrozenCallStack)

#if ( __GLASGOW_HASKELL__ >= 710 )
import           GHC.TypeLits         as X (CmpNat, KnownNat, KnownSymbol, Nat,
                                            SomeNat (..), SomeSymbol (..), Symbol, natVal,
                                            someNatVal, someSymbolVal, symbolVal)
#endif

-- Pending GHC 8.2 we'll expose these.

{-
import GHC.Records as X (
    HasField(..)
  )

<<<<<<< HEAD
=======
import Data.Kind as X (
    type (*)
  , type Type
  )
-}

#endif

-- Default Prelude defines this at the toplevel module, so we do as well.
infixr 0 $!

($!) :: (a -> b) -> a -> b
f $! x  = let !vx = x in f vx

#endif
