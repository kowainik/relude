{-# LANGUAGE Trustworthy #-}

{-
Copyright: (c) 2016 Stephen Diehl
           (c) 20016-2018 Serokell
           (c) 2018 Kowainik
License: MIT
-}

{- | Main module that reexports all functionality allowed to use
without importing any other modules. Just add next lines to your
module to replace default 'Prelude' with better one.

@
\{\-\# LANGUAGE NoImplicitPrelude \#\-\}

__import__ "Relude"
@

This documentation section contains description of internal module structure to
help navigate between modules, search for interesting functionalities and
understand where you need to put your new changes.

Functions and types are distributed across multiple modules and grouped by
meaning or __theme__. Name of the module should give you hints regarding what
this module contains. Some /themes/ contain a great amount of both reexported
functions and functions of our own. To make it easier to understand these huge
chunks of functions, all reexported stuff is moved into separate module with
name @Relude.SomeTheme.Reexport@ and our own functions and types are in
@Relude.SomeTheme.SomeName@. For example, see modules
"Relude.Container.Class" and "Relude.Container.Reexport".

Below is a short description of what you can find under different modules:

* __"Relude.Applicative"__: reexports from "Control.Applicative" and some
  general-purpose applicative combinators.
* __"Relude.Base"__: different general types and type classes from @base@
  package ('Int', 'Num', 'Generic', etc.) not exported by other modules.
* __"Relude.Bool"__: 'Bool' data type with different predicates and combinators.
* __"Relude.Debug"__: @trace@-like debugging functions with compile-time
  warnings (so you don't forget to remove them)
* __"Relude.DeepSeq"__: reexports from "Control.DeepSeq" module and
  functions to evaluate expressions to weak-head normal form or normal form.
* __"Relude.Exception"__: reexports "Control.Exception.Safe" from
  @safe-exceptions@ package, 'bug' as better 'error', 'Exc' pattern synonym for
  convenient pattern-matching on exceptions.
* __"Relude.Foldable"__: reexports functions for 'Foldable' and 'Traversable'.
* __"Relude.Function"__: almost everything from "Data.Function" module.
* __"Relude.Functor"__: reexports from "Data.Functor", "Data.Bifunctor",
  other useful 'Functor' combinators.
* __"Relude.Lifted"__: lifted to 'MonadIO' functions to work with console,
  files, 'IORef's, 'MVar's, etc.
* __"Relude.List"__: big chunk of "Data.List", 'NonEmpty' type and
  functions for this type ('head', 'tail', 'last', 'init').
* __"Relude.Monad"__: monad transormers, combinators for 'Maybe' and 'Either'.
* __"Relude.Monoid"__: reexports from "Data.Monoid" and "Data.Semigroup".
* __"Relude.Nub"__: better versions of @nub@ function for list.
* __"Relude.Print"__: polymorphic 'putStrLn' function and functions to print 'Text'.
* __"Relude.String"__: reexports from @text@ and @bytestring@ packages with
    conversion functions between different textual types.
* __"Relude.Unsafe"__: unsafe functions (produce 'error').
  Not exported by "Relude" module by default.
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
