Universum
=========

[![Build Status](https://travis-ci.org/serokell/universum.svg?branch=master)](https://travis-ci.org/serokell/universum)
[![Hackage](https://img.shields.io/hackage/v/universum.svg)](https://hackage.haskell.org/package/universum)

Prelude used in Serokell.


How to use
----------

Import `Universum`.

You should also have `NoImplicitPrelude` enabled – it's recommended to put it into the `default-extensions` section in your `.cabal` file.


Things that you were already using but now don't have to import explicitly
--------------------------------------------------------------------------

First of all, we reexport some generally useful modules: `Control.Applicative`, `Data.Traversable`, `Data.Monoid`, `Control.DeepSeq`, `Data.List`, and lots of others. Just remove unneeded imports after importing `Universum` (GHC should tell you, which ones).

Then, some commonly used types: `Map/HashMap/IntMap`, `Set/HashSet/IntSet`, `Seq`, `Text` and `ByteString` (as well as synonyms `LText` and `LByteString` for lazy versions).

`liftIO` and `MonadIO` are exported by default. Several functions are generalised to `MonadIO`.

`deepseq` is exported. For instance, if you want to force deep evaluation of some value (in IO), you can write `evaluate (force a)`. WHNF evaluation is possible with `evaluate a`.

Also we reexport big chunks of these libraries: `mtl`, `stm`, `safe`.


Gotchas
-------

* `id` is renamed to `identity` (because it's nice to be able to use `id` as a variable name).

* `head` returns `Maybe`.

* `error` triggers a compiler warning, which is likely not what you want. Either use `throwIO`, `Except`, or `panic`.

* If you try to do something like `putStrLn "hi"`, you'll get an error message if `OverloadedStrings` is enabled – it happens because the compiler doesn't know what type to infer for the string. Use `putText` in this case.

* Since `show` doesn't come from `Show` anymore, you can't write `Show` instances easily. Either use autoderived instances or `Buildable` (which isn't exported by universum yet).


Things that are missing
-----------------------

* `put` and `get` (for `MonadState`) are clashing with `Binary` so they're not exported. (Maybe we'll export some lens functions later to make up for that.)

* `head`, `tail`, `(!!)` are missing. Use `tailMay/Def/Safe` or import `unsafe(Index|Head|Tail|Init|Last)` from `Unsafe` if you need them.


Generalised functions
---------------------

* `map` is `fmap` now.

* `foreach` is `flip fmap`.

* ``sum`` and ``product`` are strict by default.

Debugging and `undefined`s
--------------------------

`trace`, `traceM`, `traceShow`, etc are available by default. GHC will warn you if you leave them in code accidentally, however. (Same for `undefined` and `error`.)

We also have `notImplemented :: a` and `data NotImplemented = NotImplemented` (which, too, come with warnings).



Text
----

We export `Text` and `LText`, and some functions work with `Text` instead of `String` – specifically, IO functions (`readFile`, `putStrLn`, etc) and `show`. In fact, `show` is polymorphic and can produce strict or lazy `Text`, `String`, or `ByteString`. Also, `toS` can convert any string type to any string type.

`error` takes `Text` (but you should use `panic` instead or throw exceptions).


Lists
-----

We export some utility functions:

* `uncons` and `unsnoc` split a list at the first/last element.

* `ordNub` is an O(n log n) version of `nub` (which is quadratic).

* `sortOn` sorts a list based on some property of its elements (e.g. `sortOn length` would sort elements by length).

* Functions from [`safe`](https://hackage.haskell.org/package/safe) – safe variants of common list/`Maybe` functions from base. `(head|tail|last|at)May` return `Maybe` instead of failing. `(head|init|last|at)Def` let you specify a default value in case of failure. `(init|tail)Safe` return an empty list in case of failure.


Other utility functions
-----------------------

* `(&)` – reverse application. `x & f & g` instead of `g $ f $ x` is useful sometimes.

* `applyN n` applies a function to a value `n` times.

* `whenM`, `unlessM`, `ifM`, `guardM` are available and do what you expect them to do (e.g. `whenM (doesFileExist "foo")`).

* `concatMapM`, too, is available and does what you expect.

* `whenJust` and `whenJustM` let you conditionally execute something if `Just`. Before:

  ```haskell
  case mbX of
      Nothing -> return ()
      Just x  -> ... x ...
  ```

  After:

  ```haskell
  whenJust mbX $ \x ->
      ... x ...
  ```

* `orEmpty` conditionally applies `pure` to something. E.g.

  ```haskell
  > orEmpty True 3
  Just 3

  > orEmpty False 3
  Nothing
  ```

  It works for any `Alternative`.

  TODO: describe `orAlt`.

* `for_` for loops. There's also `forM_` but `for_` looks a bit nicer.

  ```haskell
  for_ [1..10] $ \i -> do
      ...
  ```

* `first` and `second` apply a function to first/second part of a tuple.
  `bimap` takes two functions and applies them to first and second parts respectively.

* `readMaybe` and `readEither` are like `read` but give either `Maybe` or `Either` with parse error.

Usage
-----
To try out standalone prelude at the interactive shell, from the Protolude
project directory run.

```haskell
$ stack exec ghci
> import Universum
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
import Universum
```

Exported Functions
------------------

The list of exports is given in the [Symbols.md](./Symbols.md) file. Haddock
unfortunately breaks in the presence of module reexports and is unable to render
documentation.

License
-------

Released under the MIT License.
Copyright (c) 2016-2017, Stephen Diehl, 2017, Serokell
