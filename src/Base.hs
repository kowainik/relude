{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE Unsafe       #-}

-- | Reexports from @GHC.*@ modules of <https://www.stackage.org/lts-8.9/package/base-4.9.1.0 base>
-- package.

module Base
       ( module GHC.Base
       , module GHC.Enum
       , module GHC.Err
       , module GHC.Exts
       , module GHC.Float
       , module GHC.Num
       , module GHC.Real
       , module GHC.Show
       , module GHC.TypeLits
       , module GHC.Types

#if ( __GLASGOW_HASKELL__ >= 800 )
       , module GHC.OverloadedLabels
       , module GHC.ExecutionStack
       , module GHC.Stack
#endif

       , ($!)
       ) where

-- Base GHC types
import           GHC.Base             (String, asTypeOf, maxInt, minInt, ord, seq, (++))
import           GHC.Enum             (Bounded (..), Enum (..), boundedEnumFrom,
                                       boundedEnumFromThen)
import           GHC.Err              (error, undefined)
import           GHC.Exts             (Constraint, FunPtr, Ptr)
import           GHC.Float            (Double (..), Float (..), Floating (..), showFloat,
                                       showSignedFloat)
import           GHC.Num              (Integer, Num (..), subtract)
import           GHC.Real             hiding ((%))
import           GHC.Show             (Show (..))
import           GHC.TypeLits         (CmpNat, KnownNat, KnownSymbol, Nat, SomeNat (..),
                                       SomeSymbol (..), Symbol, natVal, someNatVal,
                                       someSymbolVal, symbolVal)
import           GHC.Types            (Bool, Char, Coercible, IO, Int, Ordering, Word)


#if ( __GLASGOW_HASKELL__ >= 800 )
import           GHC.OverloadedLabels (IsLabel (..))

import           GHC.ExecutionStack   (Location (..), SrcLoc (..), getStackTrace,
                                       showStackTrace)

import           GHC.Stack            (CallStack, HasCallStack, callStack,
                                       currentCallStack, getCallStack, prettyCallStack,
                                       prettySrcLoc, withFrozenCallStack)
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


-- | Stricter version of 'Data.Function.$' operator.
-- Default Prelude defines this at the toplevel module, so we do as well.
--
-- >>> const 3 $  undefined
-- 3
-- >>> const 3 $! undefined
-- CallStack (from HasCallStack):
--   error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
($!) :: (a -> b) -> a -> b
f $! x = let !vx = x in f vx
infixr 0 $!
