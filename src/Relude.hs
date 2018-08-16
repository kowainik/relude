{-# LANGUAGE Trustworthy #-}

{- |
Copyright: (c) 2016 Stephen Diehl
           (c) 20016-2018 Serokell
           (c) 2018 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

The main module that reexports all functionality. It's allowed to use it without
importing any other modules. If you want to use @relude@ per-module basis then
just add next lines to your module to replace default 'Prelude':

@
\{\-\# LANGUAGE NoImplicitPrelude \#\-\}

__import__ "Relude"
@

Alternatively, you can replace @base@ package in your dependencies with
@[base-noprelude](http://hackage.haskell.org/package/base-noprelude)@ and add
the following 'Prelude' module to your package to use @relude@ by default in
every module instead of 'Prelude':

@
__module__ Prelude (__module__ "Relude") __where__
__import__ "Relude"
@

This documentation section contains the description of internal module structure to
help navigate between modules, search for interesting functionalities and
understand where you need to put your new changes (if you're a contributor).

Functions and types are distributed across multiple modules and grouped by
meaning or __category__. Name of the module should give you hints regarding what
this module contains. Some /categories/ contain a significant amount of both reexported
functions and functions of our own. To make it easier to understand these enormous
chunks of functions, all reexported stuff is moved into the separate module with
name @Relude.SomeCategory.Reexport@ and our own functions and types are in
@Relude.SomeCategory.SomeName@. For example, see modules
"Relude.Foldable.Fold" and "Relude.Foldable.Reexport".

Below is a short description of what you can find under different modules,
imported by default from "Relude":

* __"Relude.Applicative"__: reexports from "Control.Applicative" and some
  general-purpose applicative combinators.
* __"Relude.Base"__: different general types and type classes from @base@
  package ('Int', 'Num', 'Generic', etc.) not exported by other modules.
* __"Relude.Bool"__: 'Bool' data type with different predicates and combinators.
* __"Relude.Container"__: 'One' typeclass for creating data structures from
  singleton lement and reexports of types from packages @containers@ and
  @unordered-containers@.
* __"Relude.Debug"__: @trace@-like debugging functions with compile-time
  warnings (so you don't forget to remove them).
* __"Relude.DeepSeq"__: reexports from "Control.DeepSeq" module and
  functions to evaluate expressions to weak-head normal form or normal form.
* __"Relude.Exception"__: reexports "Control.Exception", introduces 'bug'
  function as better 'error' and 'Exc' pattern synonym for convenient
  pattern-matching on exceptions.
* __"Relude.Foldable"__: reexports functions for 'Foldable' and 'Traversable'.
* __"Relude.Function"__: almost everything from "Data.Function" module.
* __"Relude.Functor"__: reexports from "Data.Functor", "Data.Bifunctor",
  other useful 'Functor' combinators.
* __"Relude.Lifted"__: lifted to 'MonadIO' functions to work with console,
  files, 'IORef's, 'MVar's, etc.
* __"Relude.List"__: big chunk of "Data.List", 'NonEmpty' type and
  functions for this type ('head', 'tail', 'last', 'init').
* __"Relude.Monad"__: reexports from "Data.Maybe" and "Data.Either" modules,
  monad transormers, various combinators.
* __"Relude.Monoid"__: reexports from "Data.Monoid" and "Data.Semigroup".
* __"Relude.Nub"__: better versions of @nub@ function for list.
* __"Relude.Print"__: polymorphic 'putStrLn' function and functions to print 'Text'.
* __"Relude.String"__: reexports from @text@ and @bytestring@ packages with
    conversion functions between different textual types.

And these modules are not exported by default, but you can easily bring them to
every module in your package by modifying your "Prelude" file:

* __"Relude.Extra.Bifunctor"__: additional combinators for 'Bifunctor'.
* __"Relude.Extra.CallStack"__: useful functions to extract information from
  'CallStack'.
* __"Relude.Extra.Enum"__: extra utilities for types that implement 'Bounded'
  and 'Enum' constraints.
* __"Relude.Extra.Group"__: grouping functions, polymorphic on return @Map@ type.
* __"Relude.Extra.Map"__: typeclass for @Map@-like data structures.
* __"Relude.Extra.Newtype"__: generic functions that automatically work for any
  @newtype@.
* __"Relude.Unsafe"__: unsafe partial functions (produce 'error') for lists and
  'Maybe'.
-}

module Relude
       ( -- * Reexports from base and from modules in this repo
         module Relude.Applicative
       , module Relude.Base
       , module Relude.Bool
       , module Relude.Container
       , module Relude.Debug
       , module Relude.DeepSeq
       , module Relude.Exception
       , module Relude.Foldable
       , module Relude.Function
       , module Relude.Functor
       , module Relude.Lifted
       , module Relude.List
       , module Relude.Monad
       , module Relude.Monoid
       , module Relude.Nub
       , module Relude.Print
       , module Relude.String
       ) where

import Relude.Applicative
import Relude.Base
import Relude.Bool
import Relude.Container
import Relude.Debug
import Relude.DeepSeq
import Relude.Exception
import Relude.Foldable
import Relude.Function
import Relude.Functor
import Relude.Lifted
import Relude.List
import Relude.Monad
import Relude.Monoid
import Relude.Nub
import Relude.Print
import Relude.String
