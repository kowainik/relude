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
* Type for common data structures in scope.
* Type for all common string types in scope.
* StateT/ReaderT/ExceptT transformers in scope by default.
* Foldable / Traversable functions in scope by default.
* Unsafe functions are prefixed with "unsafe" in separate module.
* Compiler agnostic, GHC internal modules are abstracted out into Base.

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
 * GHC HEAD

Usage
-----

```haskell
{-# LANGUAGE NoImplicitPrelude #-}

import Protolude
```

License
-------

Released under the MIT License.
Copyright (c) 2016, Stephen Diehl
