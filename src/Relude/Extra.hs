{- |
Copyright:  (c) 2019-2020 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Experimental
Portability: Portable

This module exports all extra-related stuff.
The extra modules are not exported by default, but you can easily bring them to
every module in your package by modifying your "Prelude" file.
-}

module Relude.Extra
    ( module Relude.Extra.Bifunctor
      -- $Bifunctor
    , module Relude.Extra.CallStack
      -- $CallStack
    , module Relude.Extra.Enum
      -- $Enum
    , module Relude.Extra.Foldable
      -- $Foldable
    , module Relude.Extra.Foldable1
      -- $Foldable1
    , module Relude.Extra.Group
      -- $Group
    , module Relude.Extra.Lens
      -- $Lens
    , module Relude.Extra.Map
      -- $Map
    , module Relude.Extra.Newtype
      -- $Newtype
    , module Relude.Extra.Tuple
      -- $Tuple
    , module Relude.Extra.Type
      -- $Type
    , module Relude.Extra.Validation
      -- $Validation
    ) where

import Relude.Extra.Bifunctor
import Relude.Extra.CallStack
import Relude.Extra.Enum
import Relude.Extra.Foldable
import Relude.Extra.Foldable1
import Relude.Extra.Group
import Relude.Extra.Lens
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Relude.Extra.Type
import Relude.Extra.Validation

{- $Bifunctor
Additional combinators for 'Relude.Bifunctor'.
-}

{- $CallStack
Useful functions to extract information from 'Relude.CallStack'.
-}
{- $Enum
Extra utilities for types that implement 'Relude.Bounded' and 'Relude.Enum'
constraints.
-}
{- $Foldable
Extra folds for instances of the 'Relude.Foldable' typeclass.
Currently, just a short-circuitable left fold 'foldlSC'.
-}

{- $Foldable1
'Foldable1' is a typeclass like 'Relude.Foldable' but for non-empty structures.
For example, 'Relude.NonEmpty', 'Relude.Identity'.

'Foldable1' has all type-safe and total methods like `head1`, `maximum1` in
contradiction with 'Data.Foldable.Foldable'.
-}

{- $Group
Grouping functions, polymorphic on return @Map@ type.
-}

{- $Lens
Minimal implementation of @lens@ package required for basic usage.
-}

{- $Map
The typeclass for @Map@-like data structures.
-}

{- $Newtype
Generic functions that automatically work for any @newtype@.
-}

{- $Tuple
Functions for working with tuples.
-}

{- $Type
Functions for inspecting and working with types.
-}

{- $Validation
'Validation' data type.

__⚠️ Warning ⚠️__
"Relude.Extra.Validation" is deprecated in favour
of [validation-selective](https://hackage.haskell.org/package/validation-selective).
-}
