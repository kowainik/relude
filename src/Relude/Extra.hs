{- |
Copyright:  (c) 2019-2020 Kowainik
SPDX-License-Identifier: MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module exports all extra-related stuff.
The extra modules are not exported by default, but you can easily bring them to
every module in your package by modifying your "Prelude" file.
-}

module Relude.Extra
    ( -- $Bifunctor
      module Relude.Extra.Bifunctor
      -- $CallStack
    , module Relude.Extra.CallStack
      -- $Enum
    , module Relude.Extra.Enum
      -- $Foldable
    , module Relude.Extra.Foldable
      -- $Foldable1
    , module Relude.Extra.Foldable1
      -- $Group
    , module Relude.Extra.Group
      -- $Lens
    , module Relude.Extra.Lens
      -- $Map
    , module Relude.Extra.Map
      -- $Newtype
    , module Relude.Extra.Newtype
      -- $Tuple
    , module Relude.Extra.Tuple
      -- $Type
    , module Relude.Extra.Type
      -- $Validation
    , module Relude.Extra.Validation
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
Extra utilities for types that implement 'Bounded' and 'Enum' constraints.
-}
{- $Foldable
Extra folds for instances of the 'Relude.Foldable' typeclass. Currently, just a short-circuitable left fold.
-}

{- $Foldable1
'Foldable1' typeclass like 'Relude.Foldable' but for non-empty structures.
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
-}
