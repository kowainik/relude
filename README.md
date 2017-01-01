Protolude
=========

[![Build Status](https://travis-ci.org/sdiehl/protolude.svg?branch=master)](https://travis-ci.org/sdiehl/protolude)
[![Hackage](https://img.shields.io/hackage/v/protolude.svg)](https://hackage.haskell.org/package/protolude)

A sensible starting Prelude for building custom Preludes.

Design points:

* Banishes String.
* Banishes partial functions.
* Compiler warning on bottoms.
* Polymorphic string IO functions.
* Polymorphic show.
* Automatic string conversions.
* Types for common data structures in scope.
* Types for all common string types (Text/ByteString) in scope.
* Banishes impure exception throwing outside of IO.
* StateT/ReaderT/ExceptT transformers in scope by default.
* Foldable / Traversable functions in scope by default.
* Unsafe functions are prefixed with "unsafe" in separate module.
* Compiler agnostic, GHC internal modules are abstracted out into Base.
* ``sum`` and ``product`` are strict by default.
* Compatibility with GHC 8.0.
* Includes Semiring for GHC >= 7.6.
* Includes Bifunctor for GHC >= 7.6.
* Includes Semigroup for GHC >= 7.6.

Supports:

 * GHC 7.6.1
 * GHC 7.6.2
 * GHC 7.6.3
 * GHC 7.8.1
 * GHC 7.8.2
 * GHC 7.8.3
 * GHC 7.8.4
 * GHC 7.10.1
 * GHC 7.10.2
 * GHC 7.10.3
 * GHC 8.0.1
 * GHC HEAD

Usage
-----

To try out standalone prelude at the interactive shell, from the Protolude
project directory run.

```haskell
$ stack exec ghci
> import Protolude
```

Swapping out the old Prelude
----------------------------

Disable the built-in prelude at the top of your file:

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
```

Or directly in your project cabal file:

```haskell
default-extensions: NoImplicitPrelude
```

Then in your modules:

```haskell
import Protolude
```

Exported Functions
------------------

The list of exports is given in the [Symbols.md](./Symbols.md) file. Haddock
unfortunately breaks in the presence of module reexports and is unable to render
documentation.

Dependencies
------------

Protolude tries to be light on dependencies and only pulls in *essential*
libraries that are universally common across most real-world projects. Lower and
upper bounds are fully specified and compatible with both vanilla Cabal and
tracks Stack LTS resolver.

| Dependencies  | Lower    | Upper    |
| -----------   | -------- | -------- |
| array         |          | 0.5      |
| async         | 2.0      | 2.2      |
| base          | 4.6      | 4.10     |
| bytestring    | 0.10     | 0.11     |
| containers    | 0.5      | 0.6      |
| deepseq       | 1.3      | 1.5      |
| ghc-prim      | 0.3      | 0.6      |
| integer-gmp   | 1.0      | 1.0      |
| mtl           | 2.1      | 2.3      |
| safe          | 0.3      | 0.4      |
| stm           | 2.4      | 2.5      |
| text          | 1.2      | 1.3      |
| hashable      | 1.2      | 1.3      |
| transformers  | 0.4      | 0.6      |

FAQs
----

* **My ``putStrLn`` and ``putStr`` instances are no longer inferred in the presense
of the ``-XOverloadedStrings`` extension?**

Because the print functions are polymorphic the type of the print functions may
require annotations if the type is not fully specified by inference. To force a
specific type at the call site use either 

```haskell
putText :: MonadIO m => T.Text -> m ()
putLText :: MonadIO m => TL.Text -> m ()
```

* **How do I write manual Show instances if ``show`` isn't provided?**

Generally speaking writing manual instances of Show is a [Haskell antipattern](
http://www.stephendiehl.com/posts/strings.html) because it produces
law-violating instances of Show. You probably want to use a [pretty
printer](https://hackage.haskell.org/package/wl-pprint-text) library for custom
printing.

If backwards compatibility is needed then the base library can be imported
manually.

```haskell
import GHC.Show (Show(..))
```

Automatic deriving of ``Show`` for your types is still supported since the class
is in scope by default.

* **Partial functions like ``undefined`` and ``error`` raise compiler warnings on
  usage.**

This is by design. For fatal uncatchable errors use the provided ``panic``
function if you intend the program to immediately abort.

```haskell
panic "Thus I die. Thus, thus, thus. Now I am dead"
```

If inside of IO simply use ``throwIO`` for exception handling, or if in pure
business logic use well-typed checked exceptions of the ``ExceptT`` variety.

* **Why is ``id`` not in scope?**

It has been renamed to ``identity`` to reserve the ``id`` identifier for the
more common use case of business logic.

License
-------

Released under the MIT License.
Copyright (c) 2016-2017, Stephen Diehl
