{-# LANGUAGE CPP  #-}
{-# LANGUAGE Safe #-}

{- |
Module                  : Relude.Functor.Reexport
Copyright               : (c) 2016 Stephen Diehl
                          (c) 2016-2018 Serokell
                          (c) 2018-2023 Kowainik
SPDX-License-Identifier : MIT
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Reexports functionality regarding 'Functor' and 'Bifunctor' typeclasses.
-}

module Relude.Functor.Reexport
    ( -- * Reexport Functor
      module Data.Functor
    , module Data.Functor.Compose
    , module Data.Functor.Identity
#if MIN_VERSION_base(4,12,0)
    , module Data.Functor.Contravariant
#endif

      -- * Reexport Bifunctor
    , module Data.Bifunctor
    ) where

import Data.Bifunctor (Bifunctor (..))
import Data.Functor (Functor (..), void, ($>), (<$>))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))

#if MIN_VERSION_base(4,12,0)
import Data.Functor.Contravariant (Comparison (..), Contravariant (..), Equivalence (..), Op (..),
                                   Predicate (..), comparisonEquivalence, defaultComparison,
                                   defaultEquivalence, phantom, ($<), (>$$<), (>$<))
#endif
