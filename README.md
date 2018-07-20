Relude
=========

[![Build Status](https://travis-ci.org/kowainik/relude.svg?branch=master)](https://travis-ci.org/kowainik/relude)
[![Hackage](https://img.shields.io/hackage/v/relude.svg)](https://hackage.haskell.org/package/relude)
[![Stackage LTS](http://stackage.org/package/relude/badge/lts)](http://stackage.org/lts/package/relude)
[![Stackage Nightly](http://stackage.org/package/relude/badge/nightly)](http://stackage.org/nightly/package/relude)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

`relude` is a custom prelude that has:

1. **Excellent documentation**: tutorial, migration guide from `Prelude`,
   Haddock with examples for (almost) every function,
   all examples are tested with [`doctest`](http://hackage.haskell.org/package/doctest),
   documenation regarding internal module structure.
2. `relude`-specific [HLint](http://hackage.haskell.org/package/hlint) rules:
   [`.hlint.yaml`](https://github.com/kowainik/relude/blob/master/.hlint.yaml)
3. Focus on safety, convenience and efficiency.

What is this file about?
------------------------

This README contains introduction to `Relude` and a tutorial on how to use it.

Structure of this tutorial
--------------------------

This tutorial has several parts:

1. [Philosophy and motivation.](#why-another-custom-prelude-)
2. [How to use `relude`.](#how-to-use-relude-)
3. [Changes in `Prelude` (some gotchas).](#gotchas-)
4. [Already known things that weren't in `Prelude` brought into scope.](#things-that-you-were-already-using-but-now-you-dont-have-to-import-them-explicitly-)
5. [New things added.](#whats-new-)
6. [Migration guide from `Prelude`.](#migration-guide-from-prelude-)

This is neither a tutorial on _Haskell_ nor tutorial on each function contained in Relude. For detailed
documentation of every function together with examples and usage, see
[_Haddock documentation_](http://hackage.haskell.org/package/relude).

Why another custom Prelude? [↑](#structure-of-this-tutorial)
---------------------------

### Motivation

We strive to be as productive as possible. That's why we are using [_Haskell_](https://haskell-lang.org/). This choice of language implies
that we're restricted to use [`Prelude`](http://hackage.haskell.org/package/base-4.9.1.0/docs/Prelude.html):
implicit import of basic functions, type classes and data types. Unfortunately, the default `Prelude`
[is considered to be not so good](https://news.ycombinator.com/item?id=8002749)
due to some historical reasons.

This is why we decided to use a better tool. Luckily, _Haskell_ provides us with the ability
to replace default `Prelude` with an alternative. All we had to do is to implement a
new basic set of defaults. There already were plenty of [preludes](https://guide.aelve.com/haskell/alternative-preludes-zr69k1hc),
so we didn't plan to implement everything from scratch.
After some long, hot discussions, our team decided to base our custom prelude on
[`protolude`](https://github.com/sdiehl/protolude). If you're not familiar with it,
you can read [a tutorial about `protolude`](http://www.stephendiehl.com/posts/protolude.html).

The next section explains why we've made this choice and what we are willing to do.
This tutorial doesn't cover the differences from `protolude`. Instead, it explains how Relude is different from regular `Prelude`.

### Main goals

While creating and maintaining a custom prelude, we are pursuing the following goals:

1. Avoid all [partial functions](https://www.reddit.com/r/haskell/comments/5n51u3/why_are_partial_functions_as_in_head_tail_bad/).
   We like [total](http://mathworld.wolfram.com/TotalFunction.html) and exception-free functions.
   You can still use some unsafe functions from `Relude.Unsafe` module,
   but they are not exported by default.
2. Use more efficient [string representations](https://www.reddit.com/r/haskell/comments/29jw0s/whats_wrong_with_string/).
   `String` type is crushingly inefficient. All our functions either try to be polymorphic over string
   type or use [`Text`](http://hackage.haskell.org/package/text-1.2.2.1/docs/Data-Text.html)
   as the default string type. Because the community is evolving slowly, some libraries still use `String` type, so `String` type alias is still reexported. We recommend to avoid `String` as much as you can!
3. Try to not reinvent the wheel. We're not trying to rebuild whole type hierarchy from scratch,
   as it's done in [`classy-prelude`](https://github.com/snoyberg/mono-traversable).
   Instead, we reexport common and well-known things from `base` and some other
   libraries that are used in everyday production programming in _Haskell_.
4. Export more useful and commonly used functions. [Hello, my name is Dmitry. I was
   coding _Haskell_ for 3 years but still hoogling which module `liftIO` comes from.](https://twitter.com/magnars/status/834683466130345984)
   Things like `liftIO`, `ReaderT` type, `MVar`-related functions have unambiguous names,
   are used in almost every non-trivial project, and it's really tedious to import them
   manually every time.

Unlike `protolude`, we are:

1. Not trying to be as general as possible (thus we don't export much from
   [`GHC.Generics`](https://github.com/sdiehl/protolude/blob/41710698eedc66fb0bfc5623d3c3a672421fbab5/src/Protolude.hs#L365)).
2. Not trying to maintain every version of `ghc` compiler (only the latest 3)
3. Trying to make writing production code easier (see
   [enhancements and fixes](https://github.com/kowainik/relude/issues)).

How to use Relude [↑](#structure-of-this-tutorial)
--------------------

Okay, enough philosophy. If you want to just start using `relude` and
explore it with the help of compiler, set everything up according to the instructions below.

If you want to get familiar with `relude` internal structure, you can just
read top-level documentation for
[`Relude`](http://hackage.haskell.org/package/relude/docs/Relude.html)
module.

### `base-noprelude`

This is the recommended way to use custom prelude. It requires you to perform
the following steps:

1. Replace `base` dependency with corresponding version of `base-noprelude` in
   your `.cabal` file.
2. Add the following `Prelude` module to your project (both file and `exposed-modules`):
   ```haskel
   module Prelude
          ( module Relude
          ) where

   import Relude
   ```
3. Optionally modify your `Prelude` to include more or less functions. Probably
   you want to hide something from `Relude` module. Or maybe you want to add
   something from `Relude.Extra.*` modules!

This is a very convenient way to add a custom prelude to your project because
you don't need to import module manually inside each file and enable the
`NoImplicitPrelude` extension.

### Per-file configuration

Disable the built-in prelude at the top of your file:

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
```

Or directly in your project `.cabal` file, if you want to use in every module by default:

```haskell
default-extensions: NoImplicitPrelude
```

Then add the following import to your modules:

```haskell
import Relude
```

Gotchas [↑](#structure-of-this-tutorial)
-------

* `head`, `tail`, `last`, `init` work with `NonEmpty a` instead of `[a]`.
* Safe analogue for `head` function: `safeHead :: [a] -> Maybe a` or you can
  use our `viaNonEmpty` function to get `Maybe a`: `viaNonEmpty head :: [a] -> Maybe a`.
* `undefined` triggers a compiler warning, which is probably not what you want. Either use `throwIO`, `Except`, `error` or `bug`.
* Multiple sorting functions are available without imports:
  + `sortBy :: (a -> a -> Ordering) -> [a] -> [a]`: sorts list using given custom comparator.
  + `sortWith :: Ord b => (a -> b) -> [a] -> [a]`: sorts a list based on some property of its elements.
  + `sortOn :: Ord b => (a -> b) -> [a] -> [a]`: just like `sortWith`, but more time-efficient if function is calculated slowly (though less space-efficient). So you should write `sortOn length` (would sort elements by length) but `sortWith fst` (would sort list of pairs by first element).
* Functions `sum` and `product` are strict now, which makes them more efficient.
* If you try to do something like `putStrLn "hi"`, you'll get an error message if
  `OverloadedStrings` is enabled – it happens because the compiler doesn't know what
  type to infer for the string. Use `putTextLn` in this case.
* Since `show` doesn't come from `Show` anymore, you can't write `Show` instances easily.
* You can't call `elem` and `notElem` functions over `Set` and `HashSet`. These
  functions are forbidden for these two types because of performance reasons.
* `error` takes `Text`.


Things that you were already using, but now you don't have to import them explicitly [↑](#structure-of-this-tutorial)
------------------------------------------------------------------------------------

### Commonly used libraries

First of all, we reexport some generally useful modules: `Control.Applicative`,
`Data.Traversable`, `Data.Monoid`, `Control.DeepSeq`, `Data.List`, and lots of others.
Just remove unneeded imports after importing `Relude` (GHC should tell you which ones).

Then, some commonly used types: `Map/HashMap/IntMap`, `Set/HashSet/IntSet`, `Seq`, `Text` and `ByteString`
(as well as synonyms `LText` and `LByteString` for lazy versions).

`liftIO` and `MonadIO` are exported by default. A lot of `IO` functions are generalized to `MonadIO`.

`deepseq` is exported. For instance, if you want to force deep evaluation of some value (in IO),
you can write `evaluateNF a`. WHNF evaluation is possible with `evaluateWHNF a`.

We also reexport big chunks of these libraries: `mtl`, `stm`.

[`Bifunctor`](http://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Bifunctor.html)
type class with useful instances is exported.

* `first` and `second` functions apply a function to first/second part of a tuple (for tuples).
* `bimap` takes two functions and applies them to first and second parts respectively.

### Text

We export `Text` and `LText`, and some functions work with `Text` instead of `String` –
specifically, IO functions (`readFile`, `putStrLn`, etc) and `show`. In fact, `show`
is polymorphic and can produce strict or lazy `Text`, `String`, or `ByteString`.
Also, `toText/toLText/toString` can convert `Text|LText|String` types to `Text/LText/String`. If you want to convert to and from `ByteString` use `encodeUtf8/decodeUtf8` functions.

### Debugging and `undefined`s

`trace`, `traceM`, `traceShow`, etc. are available by default. GHC will warn you
if you accidentally leave them in code, however (same for `undefined`).

We also have `data Undefined = Undefined` (which, too, comes with warnings).

### Exceptions

TODO: write about reexports, `Bug` and `Exc` pattern.

What's new? [↑](#structure-of-this-tutorial)
-----------

Finally, we can move to part describing the new cool features we bring with `relude`.

* `uncons` splits a list at the first element.
* `ordNub` and `sortNub` are _O(n log n)_ versions of `nub` (which is quadratic)
  and `hashNub` and `unstableNub` are almost _O(n)_ versions of `nub`.
* `(&)` – reverse application. `x & f & g` instead of `g $ f $ x` is useful sometimes.
* `whenM`, `unlessM`, `ifM`, `guardM` are available and do what you expect
  them to do (e.g. `whenM (doesFileExist "foo")`).
* Very generalized version of `concatMapM`, too, is available and does what expected.
* `readMaybe` and `readEither` are like `read` but total and give either
  `Maybe` or `Either` with parse error.
* `when(Just|Nothing|Left|Right|NotEmpty)[M][_]`
  let you conditionally execute something. Before:

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

* Conversions between `Either` and `Maybe` like `rightToMaybe` and `maybeToLeft`
  with clear semantic.
* `using(Reader|State)[T]` functions as aliases for `flip run(Reader|State)[T]`.
* [`One` type class](https://github.com/kowainik/relude/blob/master/src/Containers.hs#L473)
  for creating singleton containers. Even monomorhpic ones like `Text`.
* `evaluateWHNF` and `evaluateNF` functions as clearer and lifted aliases for
  `evaluate` and `evaluate . force`.
* `ToPairs` type class for data types that can be converted to list of pairs (like `Map` or `HashMap` or `IntMap`).

Migration guide from Prelude [↑](#structure-of-this-tutorial)
----------------------------

In order to replace default `Prelude` with `relude` you should start with instructions given in
[how to use relude](https://github.com/kowainik/relude#how-to-use-relude-) section.

This section describes what you need to change to make your code compile with `relude`.

1. Enable `-XOverloadedStrings` and `-XTypeFamilies` extension by default for your project.
2. Since `head`, `tail`, `last` and `init` work for `NonEmpty` you should
   refactor your code in one of the multiple ways described below:
   1. Change `[a]` to `NonEmpty a` where it makes sense.
   2. Use functions which return `Maybe`. There is the `viaNonEmpty` function for this.
      And you can use it like `viaNonEmpty last l`.
       + `viaNonEmpty head l` is `safeHead l`
       + `tail` is `drop 1`. It's almost never a good idea to use `tail` from `Prelude`.
   3. Add `import qualified Relude.Unsafe as Unsafe` and replace function with qualified usage.
3. If you use `fromJust` or `!!` you should use them from `import qualified Relude.Unsafe as Unsafe`.
4. If you use `foldr` or `forM_` or similar for something like `Maybe a` or
   `Either a b` it's recommended to replace usages of such function with
   monomorhpic alternatives:
   * `Maybe`
     + `(?:)          :: Maybe a -> a -> a`
     + `fromMaybe     :: a -> Maybe a -> a`
     + `maybeToList   :: Maybe a -> [a]`
     + `maybeToMonoid :: Monoid m => Maybe m -> m`
     + `maybe         :: b -> (a -> b) -> Maybe a -> b`
     + `whenJust      :: Applicative f => Maybe a -> (a -> f ()) -> f ()`
     + `whenJustM     :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()`

   * `Either`
     + `fromLeft   :: a -> Either a b -> a`
     + `fromRight  :: b -> Either a b -> b`
     + `either     :: (a -> c) -> (b -> c) -> Either a b -> c`
     + `whenRight_  :: Applicative f => Either l r -> (r -> f ()) -> f ()`
     + `whenRightM_ :: Monad m => m (Either l r) -> (r -> m ()) -> m ()`

5. Forget about `String` type.
   + Replace `putStr` and `putStrLn` with `putText` and `putTextLn`.
   + Replace `(++)` with `(<>)` for `String`-like types.
   + Try to use [`fmt`](http://hackage.haskell.org/package/fmt) library if you need to construct messages.
   + Use `toText/toLText/toString` functions to convert to `Text/LazyText/String` types.
   + Use `encodeUtf8/decodeUtf8` to convert to/from `ByteString`.
6. Run `hlint` using `.hlint.yaml` file from `relude` package to cleanup code and imports.
