{-# LANGUAGE CPP    #-}
{-# LANGUAGE Unsafe #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Reexports from @Data.*@ and @GHC.*@ modules of
<https://www.stackage.org/lts-8.9/package/base-4.9.1.0 base> package.
-}

module Relude.Base
       ( -- * Base types
         module Data.Char

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
       , module GHC.Generics
       , module GHC.Show

-- * GHC-specific functionality
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
import Data.Char (Char, chr)

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

import GHC.Base (String, asTypeOf, ord, seq, ($!))
import GHC.Enum (Bounded (..), Enum (..), boundedEnumFrom, boundedEnumFromThen)
import GHC.Generics (Generic)
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
