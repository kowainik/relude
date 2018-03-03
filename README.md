Universum
=========

[![Build Status](https://travis-ci.org/serokell/universum.svg?branch=master)](https://travis-ci.org/serokell/universum)
[![Windows build status](https://ci.appveyor.com/api/projects/status/github/serokell/universum?branch=master&svg=true)](https://ci.appveyor.com/project/ChShersh/universum)
[![Hackage](https://img.shields.io/hackage/v/universum.svg)](https://hackage.haskell.org/package/universum)
[![Stackage LTS](http://stackage.org/package/universum/badge/lts)](http://stackage.org/lts/package/universum)
[![Stackage Nightly](http://stackage.org/package/universum/badge/nightly)](http://stackage.org/nightly/package/universum)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

`universum` is a custom prelude used in @Serokell that has:

1. **Excellent documentation**: tutorial, migration guide from `Prelude`,
   Haddock with examples for (almost) every function,
   all examples are tested with [`doctest`](http://hackage.haskell.org/package/doctest).
2. `universum`-specific [HLint](http://hackage.haskell.org/package/hlint) rules:
   [`.hlint.yaml`](https://github.com/serokell/universum/blob/master/.hlint.yaml)
3. Focus on safety, convenience and efficiency.

What is this file about?
------------------------

This README contains introduction to `Universum` and a tutorial on how to use it.

Structure of this tutorial
--------------------------

This tutorial has several parts:

1. [Philosophy and motivation.](#why-another-custom-prelude-)
2. [How to use `universum`.](#how-to-use-universum-)
3. [Changes in `Prelude` (some gotchas).](#gotchas-)
4. [Already known things that weren't in `Prelude` brought into scope.](#things-that-you-were-already-using-but-now-you-dont-have-to-import-them-explicitly-)
5. [New things added.](#whats-new-)
6. [Migration guide from `Prelude`.](#migration-guide-from-prelude-)

This is not a tutorial on _Haskell_, and not even a tutorial on each function. For the detailed
documentation of every function with examples and usages see
[_Haddock documentation_](http://hackage.haskell.org/package/universum).

Why another custom Prelude? [↑](#structure-of-this-tutorial)
---------------------------

### Motivation

In [Serokell](https://github.com/serokell/),  we want to be as much productive as possible.
That's why we are using [_Haskell_](https://haskell-lang.org/). This choice of language implies
that we're restricted to use [`Prelude`](http://hackage.haskell.org/package/base-4.9.1.0/docs/Prelude.html):
implicit import of basic functions, type classes and data types. But the default `Prelude`
[is considered to be not so good](https://news.ycombinator.com/item?id=8002749)
due to some historical reasons.

This is why we decided to use a better tool. Hopefully _Haskell_ provides us with ability
to replace default `Prelude` with some alternative. All we need to do is to implement
new basic set of defaults. But we don't intend to implement everything from scratch.
There're already plenty of [preludes](https://guide.aelve.com/haskell/alternative-preludes-zr69k1hc).
After some hot long discussions our team decided to base our custom prelude on
[`protolude`](https://github.com/sdiehl/protolude). If you're not familiar with it,
you can read [a tutorial about `protolude`](http://www.stephendiehl.com/posts/protolude.html).

The next section explains why we have made this choice and what we are willing to do.
This tutorial doesn't cover the differences from `protolude`. Instead, it explains how Universum is different from custom `Prelude`.

### Main goals

While creating and maintaining a custom prelude, we are pursuing the following goals:

1. Avoid all [partial functions](https://www.reddit.com/r/haskell/comments/5n51u3/why_are_partial_functions_as_in_head_tail_bad/).
   We like [total](http://mathworld.wolfram.com/TotalFunction.html) and exception-free functions.
   Though you can still use some unsafe functions from `Universum.Unsafe` module
   but they are not exported by default.
2. Use more efficient [string representations](https://www.reddit.com/r/haskell/comments/29jw0s/whats_wrong_with_string/).
   `String` type is crushingly inefficient. All our functions either try to be polymorphic over string
   types, or use [`Text`](http://hackage.haskell.org/package/text-1.2.2.1/docs/Data-Text.html)
   as the default string type. `String` type alias is still reexported, because the community
   is evolving slowly, some libraries still use `String` type. But we recommend to avoid `String`!
3. Don't reinvent the wheel. We're not trying to rebuild whole type hierarchy from scratch,
   as it's done in [`classy-prelude`](https://github.com/snoyberg/mono-traversable).
   Instead, we reexport common and well-known things from `base` and some other
   libraries used in everyday production programming in _Haskell_.
   > **Note**: well, we did end up inventing something new.
4. Export more useful and commonly used functions. [Hello, my name is Dmitry. I was
   coding _Haskell_ for 3 years but still hoogling which module `liftIO` comes from.](https://twitter.com/magnars/status/834683466130345984)
   Things like `liftIO`, `ReaderT` type, `MVar`-related functions have unambiguous names,
   are used in almost every non-trivial project, and it's really tedious to import them
   manually every time.

Unlike `protolude`, we are:

1. Not trying to be as general as possible (thus we don't export much from
   [`GHC.Generics`](https://github.com/sdiehl/protolude/blob/41710698eedc66fb0bfc5623d3c3a672421fbab5/src/Protolude.hs#L365)).
2. Not trying to maintain every version of `ghc` compiler (only the
   [latest 3](https://github.com/serokell/universum/blob/b6353285859e9ed3544bddbf55d70237330ad64a/.travis.yml#L15)
3. Trying to make writing production code easier (see
   [enhancements and fixes](https://github.com/serokell/universum/issues)).

How to use Universum [↑](#structure-of-this-tutorial)
--------------------

Okay, enough philosophy. If you want to just start using `universum` and
explore it with the help of compiler, set everything up according to the instructions below.

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
import Universum
```

If you're using [Emacs](https://www.gnu.org/software/emacs/), you can
[modify your configs](https://github.com/serokell/universum/issues/8#issuecomment-276444879)
a little bit if you don't want to type `import Universum` manually every time.


Gotchas [↑](#structure-of-this-tutorial)
-------

* `head`, `tail`, `last`, `init` work with `NonEmpty a` instead of `[a]`.
* Safe analogue for `head` function: `safeHead :: [a] -> Maybe a`.
* `undefined` triggers a compiler warning, which is probably not what you want. Either use `throwIO`, `Except`, `error` or `bug`.
* `map` is `fmap` now.
* Multiple sorting functions are available without imports:
  + `sortBy :: (a -> a -> Ordering) -> [a] -> [a]`: sorts list using given custom comparator.
  + `sortWith :: Ord b => (a -> b) -> [a] -> [a]`: sorts a list based on some property of its elements.
  + `sortOn :: Ord b => (a -> b) -> [a] -> [a]`: just like `sortWith`, but more time-efficient if function is calculated slowly (though less space-efficient). So you should write `sortOn length` (would sort elements by length) but `sortWith fst` (would sort list of pairs by first element).
* Functions `sum` and `product` are strict now, which makes them more efficient.
* If you try to do something like `putStrLn "hi"`, you'll get an error message if
  `OverloadedStrings` is enabled – it happens because the compiler doesn't know what
  type to infer for the string. Use `putTextLn` in this case.
* Since `show` doesn't come from `Show` anymore, you can't write `Show` instances easily.
  Either use autoderived instances or
  [`Buildable`](https://github.com/serokell/universum/blob/f2ccf8afd862e37ccd204c0ef9efde48a05c2d29/src/Universum.hs#L144).
* You can't call some `Foldable` methods over `Maybe` and some other types.
  `Foldable` generalization is useful but
  [potentially error-prone](https://www.reddit.com/r/haskell/comments/60r9hu/proposal_suggest_explicit_type_application_for/).
  Instead we created our own fully compatible with `Foldable`
  [`Container` type class](https://github.com/serokell/universum/blob/b6353285859e9ed3544bddbf55d70237330ad64a/src/Universum/Container/Class.hs#L180)
  but that restricts the usage of functions like `length` over `Maybe`, `Either`, `Identity` and tuples.
  We're also using _GHC 8_ feature of
  [custom compile-time errors](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#custom-compile-time-errors)
  to produce
  [more helpful messages](https://github.com/serokell/universum/blob/54a742c10720f11c739f2d268365d723924b83a9/src/Containers.hs#L474).
* As a consequence of previous point, some functions like `traverse_`, `forM_`, `sequenceA_`, etc.
  are generalized over `Container` type classes.
* `error` takes `Text`.


Things that you were already using, but now you don't have to import them explicitly [↑](#structure-of-this-tutorial)
------------------------------------------------------------------------------------

### Commonly used libraries

First of all, we reexport some generally useful modules: `Control.Applicative`,
`Data.Traversable`, `Data.Monoid`, `Control.DeepSeq`, `Data.List`, and lots of others.
Just remove unneeded imports after importing `Universum` (GHC should tell you which ones).

Then, some commonly used types: `Map/HashMap/IntMap`, `Set/HashSet/IntSet`, `Seq`, `Text` and `ByteString`
(as well as synonyms `LText` and `LByteString` for lazy versions).

`liftIO` and `MonadIO` are exported by default. A lot of `IO` functions are generalized to `MonadIO`.

`deepseq` is exported. For instance, if you want to force deep evaluation of some value (in IO),
you can write `evaluateNF a`. WHNF evaluation is possible with `evaluateWHNF a`.

We also reexport big chunks of these libraries: `mtl`, `stm`, `microlens`, `microlens-mtl`.

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

We use [`safe-exceptions`](https://github.com/fpco/safe-exceptions)
library for exceptions handling. Don't import `Control.Exceptions`
module explicitly. Instead use functionality from `safe-exceptions`
provided by `universum` or import `Control.Exceptions.Safe` module.

What's new? [↑](#structure-of-this-tutorial)
-----------

Finally, we can move to part describing the new cool features we bring with `universum`.

* `uncons` splits a list at the first element.
* `ordNub` and `sortNub` are _O(n log n)_ versions of `nub` (which is quadratic)
  and `hashNub` and `unstableNub` are almost _O(n)_ versions of `nub`.
* `(&)` – reverse application. `x & f & g` instead of `g $ f $ x` is useful sometimes.
* `pretty` and `prettyL` for converting `Buildable` into `Text` (suggested be used instead of `show`).
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
  for creating singleton containers. Even monomorhpic ones like `Text`.
* `evaluateWHNF` and `evaluateNF` functions as clearer and lifted aliases for
  `evaluate` and `evaluate . force`.
* `ToPairs` type class for data types that can be converted to list of pairs (like `Map` or `HashMap` or `IntMap`).

Migration guide from Prelude [↑](#structure-of-this-tutorial)
----------------------------

In order to replace default `Prelude` with `universum` you should start with instructions given in
[how to use universum](https://github.com/serokell/universum#how-to-use-universum) section.

This section describes what you need to change to make your code compile with `universum`.

1. Enable `-XOverloadedStrings` and `-XTypeFamilies` extension by default for your project.
2. Since `head`, `tail`, `last` and `init` work for `NonEmpty` you should
   refactor your code in one of the multiple ways described below:
   1. Change `[a]` to `NonEmpty a` where it makes sense.
   2. Use functions which return `Maybe`. They can be implemented using `nonEmpty` function. Like `head <$> nonEmpty l`.
       + `head <$> nonEmpty l` is `safeHead l`
       + `tail` is `drop 1`. It's almost never a good idea to use `tail` from `Prelude`.
   3. Add `import qualified Universum.Unsafe as Unsafe` and replace function with qualified usage.
3. If you use `fromJust` or `!!` you should use them from `import qualified Universum.Unsafe as Unsafe`.
4. Derive or implement `Container` instances for your data types which implement
   `Foldable` instances. This can be done in a single line because `Container`
   type class automatically derives from `Foldable`.
5. `Container` type class from `universum` replaces `Foldable` and doesn't have
   instances for `Maybe a`, `(a, b)`, `Identity a` and `Either a b`. If you use
   `foldr` or `forM_` or similar for something like `Maybe a` you should replace
   usages of such function with monomorhpic alternatives:
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
     + `whenRight  :: Applicative f => Either l r -> (r -> f ()) -> f ()`
     + `whenRightM :: Monad m => m (Either l r) -> (r -> m ()) -> m ()`

6. If you have types like `foo :: Foldable f => f a -> a -> a` you should chose one of the following:
   + `Right`: Modify types for `Container` like `foo :: (Container t, Element t ~ a) => t -> a -> a`.
   + `Left`: Import `Data.Foldable` module `qualified` and use everything `Foldable`-related qualified.
7. Forget about `String` type.
   + Replace `putStr` and `putStrLn` with `putText` and `putTextLn`.
   + Replace `(++)` with `(<>)` for `String`-like types.
   + Try to use [`fmt`](http://hackage.haskell.org/package/fmt) library if you need to construct messages.
   + Use `toText/toLText/toString` functions to convert to `Text/LazyText/String` types.
   + Use `encodeUtf8/decodeUtf8` to convert to/from `ByteString`.
8. Run `hlint` using `.hlint.yaml` file from `universum` package to cleanup code and imports.

Projects that use Universum [↑](#structure-of-this-tutorial)
---------------------------

- [cardano-report-server](https://github.com/input-output-hk/cardano-report-server)
- [cardano-sl](https://github.com/input-output-hk/cardano-sl)
- [importify](https://github.com/serokell/importify)
- [log-warper](https://github.com/serokell/log-warper)
- [orgstat](https://github.com/volhovm/orgstat)
