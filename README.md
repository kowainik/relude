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
* Types for all common string types in scope.
* StateT/ReaderT/ExceptT transformers in scope by default.
* Foldable / Traversable functions in scope by default.
* Unsafe functions are prefixed with "unsafe" in separate module.
* Compiler agnostic, GHC internal modules are abstracted out into Base.
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

```haskell
{-# LANGUAGE NoImplicitPrelude #-}

import Protolude
```

Dependencies
------------

Protolude tries to be light on dependencies and only pulls in *essential*
libraries that are universally common across most real-world projects. 

Dependencies  Bound
-----------   --------
array         0.5.1.0
async         2.1.0
base          4.8.2.0
binary        0.7.5.0
bytestring    0.10.6.0
containers    0.5.6.2
deepseq       1.4.1.1
ghc-prim      0.4.0.0
integer-gmp   1.0.0.0
mtl           2.2.1
protolude     0.1.6
safe          0.3.9
stm           2.4.4.1
text          1.2.2.1
transformers  0.4.2.0

License
-------

Released under the MIT License.
Copyright (c) 2016, Stephen Diehl
