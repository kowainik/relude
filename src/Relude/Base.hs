{-# LANGUAGE CPP    #-}
{-# LANGUAGE Unsafe #-}

{- |
Copyright: (c) 2016 Stephen Diehl
           (c) 20016-2018 Serokell
           (c) 2018 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Reexports from @Data.*@ and @GHC.*@ modules of
<https://www.stackage.org/lts-8.9/package/base-4.9.1.0 base> package.
-}

module Relude.Base
       ( -- * Base types
         module Data.Bits
       , module Data.Char
       , module Data.Int
       , module Data.Word
       , Natural

         -- * Base type classes
       , module Data.Eq
       , module Data.Ord

         -- * System IO
       , module System.IO

         -- * Types for type-level computation
       , module Data.Coerce
       , module Data.Kind
       , module Data.Proxy
       , module Data.Typeable
       , module Data.Void

       , module GHC.Base
       , module GHC.Enum
       , module GHC.Float
       , module GHC.Generics
       , module GHC.Num
       , module GHC.Real
       , module GHC.Show

#if MIN_VERSION_base(4,10,0)
       , module GHC.TypeNats
#else
       , module GHC.TypeLits
#endif

       , module GHC.OverloadedLabels
       , module GHC.ExecutionStack
       , module GHC.Stack
       ) where

-- Base types
import Data.Bits (xor)
import Data.Char (Char, chr)
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Word (Word, Word16, Word32, Word64, Word8, byteSwap16, byteSwap32, byteSwap64)
import Numeric.Natural (Natural)

-- IO
import System.IO (FilePath, Handle, IO, IOMode (..), stderr, stdin, stdout, withFile)

-- Base typeclasses
import Data.Eq (Eq (..))
import Data.Ord (Down (..), Ord (..), Ordering (..), comparing)

-- Types for type-level computation
import Data.Coerce (Coercible, coerce)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Void (Void, absurd, vacuous)

import GHC.Base (String, asTypeOf, maxInt, minInt, ord, seq, ($!))
import GHC.Enum (Bounded (..), Enum (..), boundedEnumFrom, boundedEnumFromThen)
import GHC.Float (Double (..), Float (..), Floating (acos, acosh, asin, asinh, atan, atanh, cos, cosh, exp, logBase, pi, sin, sinh, sqrt, tan, tanh, (**)))
import GHC.Generics (Generic)
import GHC.Num (Integer, Num (..), subtract)
import GHC.Real (Fractional (..), Integral (..), Ratio, Rational, Real (..), RealFrac (..),
                 denominator, even, fromIntegral, gcd, lcm, numerator, odd, realToFrac, (^), (^^))
import GHC.Show (Show)

#if MIN_VERSION_base(4,10,0)
import GHC.TypeNats (CmpNat, KnownNat, Nat, SomeNat (..), natVal, someNatVal)
#else
import GHC.TypeLits (CmpNat, KnownNat, Nat, SomeNat (..), natVal, someNatVal)
#endif

import GHC.ExecutionStack (getStackTrace, showStackTrace)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Stack (CallStack, HasCallStack, callStack, currentCallStack, getCallStack,
                  prettyCallStack, prettySrcLoc, withFrozenCallStack)
