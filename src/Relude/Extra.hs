{- |
Module                  : Relude.Extra
Copyright               : (c) 2019-2023 Kowainik
SPDX-License-Identifier : MIT
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

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

{- $Bifunctor
Additional combinators for t'Relude.Functor.Reexport.Bifunctor'.
-}

{- $CallStack
Useful functions to extract information from t'Relude.Base.CallStack'.
-}
{- $Enum
Extra utilities for types that implement t'Relude.Enum.Bounded' and t'Relude.Enum.Enum'
constraints.
-}
{- $Foldable
Extra folds for instances of the t'Relude.Foldable.Reexport.Foldable' typeclass.
Currently, just a short-circuitable left fold 'foldlSC'.
-}

{- $Foldable1
'Foldable1' is a typeclass like t'Relude.Foldable.Reexport.Foldable' but for non-empty
structures.  For example, t'Relude.List.NonEmpty.NonEmpty',
'Relude.Functor.Reexport.Identity'.

'Foldable1' has all type-safe and total methods like `head1`, `maximum1` in
contradiction with t'Relude.Foldable.Reexport.Foldable'.
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
