{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE Unsafe             #-}

module Base
       ( module X
       , ($!)
       ) where

-- Glorious Glasgow Haskell Compiler
#if defined(__GLASGOW_HASKELL__) && ( __GLASGOW_HASKELL__ >= 600 )

-- Base GHC types
import           GHC.Base             as X (asTypeOf, maxInt, minInt, ord, seq, (++))
import           GHC.Enum             as X
import           GHC.Err              as X (error, undefined)
import           GHC.Exts             as X (Constraint, FunPtr, Ptr)
import           GHC.Float            as X
import           GHC.Num              as X
import           GHC.Real             as X hiding ((%))
import           GHC.Show             as X (Show (..))
import           System.IO            as X (print, putStr, putStrLn)

import           GHC.Types            as X (Bool, Char, Coercible, IO, Int, Ordering,
                                            Word)

#if ( __GLASGOW_HASKELL__ >= 800 )
import           GHC.OverloadedLabels as X (IsLabel (..))

import           GHC.ExecutionStack   as X (Location (..), SrcLoc (..), getStackTrace,
                                            showStackTrace)

import           GHC.Stack            as X (CallStack, HasCallStack, callStack,
                                            currentCallStack, getCallStack,
                                            prettyCallStack, prettySrcLoc)

{-
import GHC.Records as X (
    HasField(..)
  )
-}

#endif

infixr 0 $!

($!) :: (a -> b) -> a -> b
f $! x  = let !vx = x in f vx

#endif
