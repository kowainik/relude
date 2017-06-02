Universum
=========

[![Build Status](https://travis-ci.org/serokell/universum.svg?branch=master)](https://travis-ci.org/serokell/universum)
[![Hackage](https://img.shields.io/hackage/v/universum.svg)](https://hackage.haskell.org/package/universum)

Custom prelude used in Serokell.

What is this?
-------------

This README is the introduction to `Universum` and tutorial how to use it.

Structure of this tutorial
--------------------------

This tutorial has several parts:

1. Philosophy and motivation.
2. How to use it.
3. What we've changed in `Prelude` (some gotchas).
4. What already known things that are not in `Prelude` we've brought into scope.
5. What new things we've added.

This is not a tutorial on _Haskell_ or not even tutorial on each function. For detailed
documentation of every function with examples and usages see
[_Haddock documentation_](http://hackage.haskell.org/package/universum).

Why another custom Prelude?
---------------------------

### Motivation

In [Serokell](https://github.com/serokell/) we want to be as much productive as possible.
That's why we are using [_Haskell_](https://haskell-lang.org/). This choice implies
that we're restricted to use [`Prelude`](http://hackage.haskell.org/package/base-4.9.1.0/docs/Prelude.html):
impicit import of basic functions, type classes and data type. But default `Prelude`
[is considered to be not so good](https://news.ycombinator.com/item?id=8002749)
due to some historical reasons.

So we decided to use better tool. Hopefully _Haskell_ provides us with ability
to replace default `Prelude` with some alternative. All we need to do is to implement
new basic set of defaults. But we don't actually want to implement everything from scratch.
There're plenty of [already created custom preludes](https://guide.aelve.com/haskell/alternative-preludes-zr69k1hc).
After long and hot discussion our team decided to base our custom prelude on
[`protolude`](https://github.com/sdiehl/protolude). If you're not familiar with it
you can read [tutorial about `protolude`](http://www.stephendiehl.com/posts/protolude.html).

Next section describes why we made this choice and what we're willing to do.
This tutorial doesn't cover differences from `protolude`. Intstead it says what
differs from custom `Prelude`.

### Main goals

While creating and maintaining custom prelude we're following next goals:

1. Avoid all [partial functions](https://www.reddit.com/r/haskell/comments/5n51u3/why_are_partial_functions_as_in_head_tail_bad/).
   We like [total](http://mathworld.wolfram.com/TotalFunction.html) and exception-free functions.
   You can still use some [_unsafe_](https://github.com/serokell/universum/blob/master/src/Unsafe.hs)
   functions but they are not exported by default.
2. Use more efficient [string representations](https://www.reddit.com/r/haskell/comments/29jw0s/whats_wrong_with_string/).
   `String` type is crushingly inefficient. All our functions tries to be polymorphic over string
   types or use [`Text`](http://hackage.haskell.org/package/text-1.2.2.1/docs/Data-Text.html)
   as the default string type. Though `String` type alias is still reexported because community
   is evolving slowly, some libraries still use `String` type. But we recommend to avoid `String`!
3. Don't reinvent wheels. We're not trying to rebuild whole type hierarchy from scratch
   as it's done in [`classy-prelude`](https://github.com/snoyberg/mono-traversable).
   Instead we just reexport common and well-known things from `base` and some other
   libraries used in everyday production programming in _Haskell_.
   > **Note**: well, we didn't managed to avoid inventing something new, but just a little bit.
4. Export more useful and commonly used functions. [Hello, my name is Dmitry. I was
   coding _Haskell_ for 3 years but still hoogling which module `liftIO` comes from.](https://twitter.com/magnars/status/834683466130345984)
   Things like `liftIO`, `ReaderT` type, `MVar`-related functions have unambigous names,
   used in almost every non-trivial project and it's really tedious to import them
   manually every time.

Unlike `protolude` we're:

1. Not trying to be as general as possible (thus we don't export much from
   [`GHC.Generics`](https://github.com/sdiehl/protolude/blob/41710698eedc66fb0bfc5623d3c3a672421fbab5/src/Protolude.hs#L365)).
2. Not trying to maintain every version of `ghc` compiler (only
   [latest 3](https://github.com/serokell/universum/blob/f2ccf8afd862e37ccd204c0ef9efde48a05c2d29/.travis.yml#L3)).
3. But trying to be able to write production code easier (see
   [enhancements and fixes](https://github.com/serokell/universum/issues)).

How to use
----------

Okay, enough philosophy. If you want to just start using `universum` and
explore it with the help of compiler just set up according to the instructions bellow.

Disable the built-in prelude at the top of your file:

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
```

Or directly in your project `.cabal` file if you want to use in every module by default:

```haskell
default-extensions: NoImplicitPrelude
```

Then in your modules add next import:

```haskell
import Universum
```

If you're using [Emacs](https://www.gnu.org/software/emacs/), you can
[modify your configs](https://github.com/serokell/universum/issues/8#issuecomment-276444879)
a little bit if you don't want to write `import Universum` manually every time.


Gotchas
-------

* `id` is renamed to `identity` (because it's nice to be able to use `id` as a variable name).
* `head` returns `Maybe`.
* `tail`, `last`, `init`, `(!!)` are missing. Use `tailMay/Def/Safe` or import
  `unsafe(Index|Head|Tail|Init|Last)` from `Unsafe` if you need them.
* `undefined` triggers a compiler warning, which is likely not what you want. Either use `throwIO`, `Except`, or `error`.
* `map` is `fmap` now.
* `sortOn` availaible without import. This function sorts efficiently a list based on some
  property of its elements (e.g. `sortOn length` would sort elements by length).
* Functions `sum` and `product` are strict now which makes them more efficient.
* If you try to do something like `putStrLn "hi"`, you'll get an error message if
  `OverloadedStrings` is enabled – it happens because the compiler doesn't know what
  type to infer for the string. Use `putText` in this case.
* Since `show` doesn't come from `Show` anymore, you can't write `Show` instances easily.
  Either use autoderived instances or
  [`Buildable`](https://github.com/serokell/universum/blob/f2ccf8afd862e37ccd204c0ef9efde48a05c2d29/src/Universum.hs#L144).
* You can't call some `Foldable` methods over `Maybe` and some other types.
  `Foldable` generalization is useful but
  [potentially error-prone](https://www.reddit.com/r/haskell/comments/60r9hu/proposal_suggest_explicit_type_application_for/).
  Instead we created our own fully compatible with `Foldable`
  [`Container` type class](https://github.com/serokell/universum/blob/54a742c10720f11c739f2d268365d723924b83a9/src/Containers.hs)
  but that restrics usages of functions like `length` over `Maybe`, `Either`, `Identity` and tuples.
  We're also using _GHC 8_ feature of
  [custom compile-time errors](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#custom-compile-time-errors)
  to produce
  [more helpful messages](https://github.com/serokell/universum/blob/54a742c10720f11c739f2d268365d723924b83a9/src/Containers.hs#L474).
* As a consequence of previous point, some functions like `traverse_`, `forM_`, `sequenceA_`, etc
  are generalized over `Container` and `NonTrivialContainer` type classes.
* `error` takes `Text`.


Things that you were already using but now don't have to import explicitly
--------------------------------------------------------------------------

### Commonly used libraries

First of all, we reexport some generally useful modules: `Control.Applicative`,
`Data.Traversable`, `Data.Monoid`, `Control.DeepSeq`, `Data.List`, and lots of others.
Just remove unneeded imports after importing `Universum` (GHC should tell you, which ones).

Then, some commonly used types: `Map/HashMap/IntMap`, `Set/HashSet/IntSet`, `Seq`, `Text` and `ByteString`
(as well as synonyms `LText` and `LByteString` for lazy versions).

`liftIO` and `MonadIO` are exported by default. A lot of functions are generalised to `MonadIO`.

`deepseq` is exported. For instance, if you want to force deep evaluation of some value (in IO),
you can write `evaluateNF a`. WHNF evaluation is possible with `evaluateWHNF a`.

Also we reexport big chunks of these libraries: `mtl`, `stm`, `safe`, `microlens`, `microlens-mtl`.

However, `put` and `get` (for `MonadState`) are clashing with `Binary` so they're not exported.

More precisely about functions from [`safe`](https://hackage.haskell.org/package/safe):
we bring into scope safe variants of common list/`Maybe` functions from base.

* `(head|tail|last|at)May` return `Maybe` instead of failing.
* `(head|init|last|at)Def` let you specify a default value in case of failure.
* `(init|tail)Safe` return an empty list in case of failure.

However, there's still [pending issue](https://github.com/serokell/universum/issues/5) about some enhancements.

[`Bifunctor`](http://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Bifunctor.html)
type class with useful instances is exported.

* `first` and `second` functions apply a function to first/second part of a tuple (for tuples).
* `bimap` takes two functions and applies them to first and second parts respectively.

### Text

We export `Text` and `LText`, and some functions work with `Text` instead of `String` –
specifically, IO functions (`readFile`, `putStrLn`, etc) and `show`. In fact, `show`
is polymorphic and can produce strict or lazy `Text`, `String`, or `ByteString`.
Also, `toS` can convert any string type to any string type.
But you can use `toText`, `toByteString`, `toString` functions to convert to any specific type.

### Debugging and `undefined`s

`trace`, `traceM`, `traceShow`, etc are available by default. GHC will warn you
if you leave them in code accidentally, however (same for `undefined`).

We also have `data Undefined = Undefined` (which, too, come with warnings).


What's new?
-----------

Finally, we can move to part describing what new cool features we bring with `universum`.

* `uncons` and `unsnoc` split a list at the first/last element.
* `ordNub` is an _O(n log n)_ version of `nub` (which is quadratic) and `hashNub` is almost
  an _O(n)_ version of `nub`.
* `(&)` – reverse application. `x & f & g` instead of `g $ f $ x` is useful sometimes.
* `pretty` and `prettyL` for converting `Buildable` into `Text` (can be used instead of `show`).
* `whenM`, `unlessM`, `ifM`, `guardM` are available and do what you expect
  them to do (e.g. `whenM (doesFileExist "foo")`).
* Very generalized version of `concatMapM`, too, is available and does what you expect.
* `readMaybe` and `readEither` are like `read` but total and give either
  `Maybe` or `Either` with parse error.
* `when(Just|Nothing|Left|Right|NotEmpty)[M][_]`
  let you conditionally execute something. For example, before:

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

* `for_` for loops. There's also `forM_` but `for_` looks a bit nicer.

  ```haskell
  for_ [1..10] $ \i -> do
      ...
  ```

* `andM`, `allM`, `anyM`, `orM` are monadic version of corresponding functions from `base`.
* Type operator `$` for writing types like `Maybe $ Either String $ Maybe Int`.
* `Each` type family. So this:

  ```haskell
  f :: Each [Show, Read] [a, b] => a -> b -> String
  ```

  translates into this:

  ```haskell
  f :: (Show a, Show b, Read a, Read b) => a -> b -> String
  ```

* `With` type operator. So this:

  ```haskell
  a :: With [Show, Read] a => a -> a
  ```

  translates into this:

  ```haskell
  a :: (Show a, Read a) => a -> a
  ```

* Variadic composition operator `(...)`. So you can write:

  ```haskell
  ghci> (show ... (+)) 1 2
  "3"
  ghci> show ... 5
  "5"
  ghci> (null ... zip5) [1] [2] [3] [] [5]
  True
  ghci> let process = map (+3) ... filter
  ghci> process even [1..5]
  [5,7]
  ```

* Conversions between `Either` and `Maybe` like `rightToMaybe` and `maybeToLeft`
  with clear semantic.
* `using(Reader|State)[T]` functions as aliases for `flip run(Reader|State)[T]`.
* [`One` type class](https://github.com/serokell/universum/blob/master/src/Containers.hs#L473)
  for creating singleton containers. Even monomorhpic like `Text`.
* `evaluateWHNF` and `evaluateNF` functions as clearer and lifted aliases for
  `evaluate` and `evaluate . force`.

License
-------

Released under the MIT License.
Copyright (c) 2016, Stephen Diehl, 2016-2017, Serokell
