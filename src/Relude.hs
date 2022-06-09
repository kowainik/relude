{-# LANGUAGE Safe #-}

{- |
Module                  : Relude
Copyright               : (c) 2016 Stephen Diehl
                          (c) 2016-2018 Serokell
                          (c) 2018-2022 Kowainik
SPDX-License-Identifier : MIT
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

@relude@ is a safe, performant, user-friendly and lightweight Haskell
standard library.

"Relude" is the main module that reexports all functionality provided by the library that will replace the default @Prelude@ in your project.

== Usage

To start using @relude@ in your project, you can set the library up for you by
one of the following ways.

=== mixins

One of the most convenient ways to use @relude@ is via the @mixins@ feature. This
feature is available since @Cabal >= 2.2@. In order to use the @mixins@ feature
one needs to specify supported @cabal-version@ in your package description.
And then the following lines should be added to the required stanza to replace
default "Prelude" with @relude@.

@
cabal-version: 2.4
...
library
  ...
  mixins: base hiding (Prelude)
        , relude (Relude as Prelude)
        , relude
@

=== @base-noprelude@

Alternatively, you can replace @base@ package in your dependencies with
@[base-noprelude](http://hackage.haskell.org/package/base-noprelude)@ and add
the following "Prelude" module to your package to use @relude@ by default in
every module instead of @base@ "Prelude":

@
__module__ Prelude
    ( __module__ "Relude"
    ) __where__

__import__ "Relude"
@

=== @NoImplicitPrelude@

If you want to use @relude@ per-module basis then just add next lines to your
module to replace default "Prelude":

@
\{\-\# LANGUAGE NoImplicitPrelude \#\-\}

__import__ "Relude"
@

== Structure

This documentation section contains the description of internal module structure to
help navigate between modules, search for interesting functionalities and
understand where you need to put your new changes (if you're a contributor).

Functions and types are distributed across multiple modules and grouped by
meaning or __category__. Name of the module should give you hints regarding what
this module contains. Some /categories/ contain a significant amount of both
reexported
functions and functions of our own. To make it easier to understand these enormous
chunks of functions, all reexported stuff is moved into the separate module with
name @Relude.SomeCategory.Reexport@ and our own functions and types are in
@Relude.SomeCategory.SomeName@. For example, see modules
"Relude.Foldable.Fold" and "Relude.Foldable.Reexport".
-}

module Relude
    ( -- * Default Modules
      module Relude.Applicative
      -- $applicative
    , module Relude.Base
      -- $base
    , module Relude.Bool
      -- $bool
    , module Relude.Container
      -- $container
    , module Relude.Debug
      -- $debug
    , module Relude.DeepSeq
      -- $deepseq
    , module Relude.Enum
      -- $enum
    , module Relude.Exception
      -- $exception
    , module Relude.File
      -- $file
    , module Relude.Foldable
      -- $foldable
    , module Relude.Function
      -- $function
    , module Relude.Functor
      -- $functor
    , module Relude.Lifted
      -- $lifted
    , module Relude.List
      -- $list
    , module Relude.Monad
      -- $monad
    , module Relude.Monoid
      -- $monoid
    , module Relude.Nub
      -- $nub
    , module Relude.Numeric
      -- $numeric
    , module Relude.Print
      -- $print
    , module Relude.String
      -- $string

      -- * Extra Modules
      -- $extra
    ) where

import Relude.Applicative
import Relude.Base
import Relude.Bool
import Relude.Container
import Relude.Debug
import Relude.DeepSeq
import Relude.Enum
import Relude.Exception
import Relude.File
import Relude.Foldable
import Relude.Function
import Relude.Functor
import Relude.Lifted
import Relude.List
import Relude.Monad
import Relude.Monoid
import Relude.Nub
import Relude.Numeric
import Relude.Print
import Relude.String

{- $applicative
__"Relude.Applicative"__ contains reexports from "Control.Applicative" and some
general-purpose applicative combinators.
-}

{- $base
__"Relude.Base"__ contains different general types and type classes from @base@
package ('Char', 'Eq', 'Generic', etc.) not exported by other modules.
-}

{- $bool
__"Relude.Bool"__ contains 'Bool' data type with different predicates and combinators.
-}

{- $container
__"Relude.Container"__ provides 'One' typeclass for creating data structures
from singleton element and reexports of types from packages @containers@ and
@unordered-containers@.
-}

{- $debug
__"Relude.Debug"__ contains @trace@-like debugging functions with compile-time
warnings (so you don't forget to remove them).
-}

{- $deepseq
__"Relude.DeepSeq"__ has reexports from "Control.DeepSeq" module and functions
to evaluate expressions to weak-head normal form or normal form.
-}

{- $enum
__"Relude.Enum"__ reexports 'Enum' related typeclasses and functions. Also
introduced a few useful helpers to work with Enums.
-}

{- $exception
__"Relude.Exception"__ contains reexports from "Control.Exception", introduces
'bug' function as better 'error' and 'Exc' pattern synonym for convenient
pattern-matching on exceptions.
-}

{- $file
__"Relude.File"__ implements functions to work with file content as 'Text' or
'ByteString'.
-}

{- $foldable
__"Relude.Foldable"__ reexports functions for 'Foldable' and 'Traversable' and
provide own better alternatives to some existing functions.
-}

{- $function
__"Relude.Function"__ contains almost everything from the "Data.Function" module.
-}

{- $functor
__"Relude.Functor"__ contains reexports from "Data.Functor", "Data.Bifunctor",
other useful 'Functor' combinators.
-}

{- $lifted
__"Relude.Lifted"__ implements lifted to 'MonadIO' functions to work with
console, files, 'IORef's, 'MVar's, etc.
-}

{- $list
__"Relude.List"__ provides big chunk of "Data.List", 'NonEmpty' type and
functions for this type ('head', 'tail', 'last', 'init').
-}

{- $monad
__"Relude.Monad"__ contains functions and data types from "Data.Maybe" and
"Data.Either" modules, monad transformers and other various combinators.
-}

{- $monoid
__"Relude.Monoid"__ reexports various types and functions from "Data.Monoid" and
"Data.Semigroup".
-}

{- $nub
__"Relude.Nub"__ implements better versions of @nub@ function for list.
-}

{- $numeric
__"Relude.Numeric"__ contains functions and types to work with numerical data.
-}

{- $print
__"Relude.Print"__ contains printing to terminal functions for 'Text' and 'ByteString'.
-}

{- $string
__"Relude.String"__ contains reexports from @text@ and @bytestring@ packages
with conversion functions between different textual types.
-}

{- $extra
The following modules are not exported by default, but you can easily bring them to
every module in your package by modifying your "Prelude" file:

+------------------------------+-----------------------------------------------------------+
| __"Relude.Extra"__           | Reexports every @Relude.Extra.*@ module                   |
+------------------------------+-----------------------------------------------------------+
| __"Relude.Extra.Bifunctor"__ | Additional combinators for 'Bifunctor'.                   |
+------------------------------+-----------------------------------------------------------+
| __"Relude.Extra.CallStack"__ | Useful functions to extract information from 'CallStack'. |
+------------------------------+-----------------------------------------------------------+
| __"Relude.Extra.Enum"__      | Extra utilities for types that implement 'Bounded'        |
|                              | and 'Enum' constraints.                                   |
+------------------------------+-----------------------------------------------------------+
| __"Relude.Extra.Foldable"__  | Extra folds for instances of the 'Foldable' typeclass.    |
|                              | Currently, just a short-circuitable left fold.            |
+------------------------------+-----------------------------------------------------------+
| __"Relude.Extra.Foldable1"__ | 'Relude.Extra.Foldable1.Foldable1' typeclass              |
|                              | like 'Foldable' but for non-empty structures.             |
+------------------------------+-----------------------------------------------------------+
| __"Relude.Extra.Group"__     | Grouping functions, polymorphic on return @Map@ type.     |
+------------------------------+-----------------------------------------------------------+
| __"Relude.Extra.Lens"__      | Minimal implementation of @lens@ package required         |
|                              | for basic usage.                                          |
+------------------------------+-----------------------------------------------------------+
| __"Relude.Extra.Map"__       | Typeclass for @Map@-like data structures.                 |
+------------------------------+-----------------------------------------------------------+
| __"Relude.Extra.Newtype"__   | Generic functions that automatically work for any         |
|                              | @newtype@.                                                |
+------------------------------+-----------------------------------------------------------+
| __"Relude.Extra.Tuple"__     | Functions for working with tuples.                        |
+------------------------------+-----------------------------------------------------------+
| __"Relude.Extra.Type"__      | Functions for inspecting and working with types.          |
+------------------------------+-----------------------------------------------------------+
| __"Relude.Unsafe"__          | Unsafe partial functions (produce 'error') for lists and  |
|                              | 'Maybe'.                                                  |
+------------------------------+-----------------------------------------------------------+

-}
