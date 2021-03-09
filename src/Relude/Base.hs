{-# LANGUAGE Trustworthy #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2021 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

Reexports from @Data.*@ and @GHC.*@ modules of
<https://hackage.haskell.org/package/base base> package.
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

      -- * Basic type classes
    , module GHC.Base
    , module GHC.Enum
    , module GHC.Generics
    , module GHC.Show

-- * GHC-specific functionality
    , module GHC.TypeNats
    , module GHC.OverloadedLabels
    , module GHC.ExecutionStack
    , module GHC.Stack
    ) where

-- Base types
import Data.Char (Char, chr)

-- IO
import System.IO (FilePath, IO, IOMode (..))

-- Base typeclasses
import Data.Eq (Eq (..))
import Data.Ord (Down (..), Ord (..), Ordering (..), comparing)

-- Types for type-level computation
import Data.Coerce (Coercible, coerce)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Void (Void, absurd, vacuous)

import GHC.Base (asTypeOf, ord, seq, ($!))
import GHC.Enum (Bounded (..), Enum (..), boundedEnumFrom, boundedEnumFromThen)
import GHC.Generics (Generic)
import GHC.Show (Show)

import GHC.TypeNats (CmpNat, KnownNat, Nat, SomeNat (..), natVal, someNatVal)

import GHC.ExecutionStack (getStackTrace, showStackTrace)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Stack (CallStack, HasCallStack, callStack, currentCallStack, getCallStack,
                  prettyCallStack, prettySrcLoc, withFrozenCallStack)
