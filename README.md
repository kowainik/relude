# Relude

[![Build Status](https://travis-ci.org/kowainik/relude.svg?branch=master)](https://travis-ci.org/kowainik/relude)
[![Hackage](https://img.shields.io/hackage/v/relude.svg)](https://hackage.haskell.org/package/relude)
[![Stackage LTS](http://stackage.org/package/relude/badge/lts)](http://stackage.org/lts/package/relude)
[![Stackage Nightly](http://stackage.org/package/relude/badge/nightly)](http://stackage.org/nightly/package/relude)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

`relude` is a custom prelude based on `universum`. `relude` tries to achieve the following goals:

1. **Avoid all** [**partial functions**](https://www.reddit.com/r/haskell/comments/5n51u3/why_are_partial_functions_as_in_head_tail_bad/)
   (like `head :: [a] -> a`). The types of partial functions lie about their
   behavior and usage of such functions can lead to the unexpected bugs. Though
   you can still use some unsafe functions from `Relude.Unsafe` module, but they
   are not exported by default.
2. **Type-safety**. We like to make invalid states unrepresantable. And if it's
   possible to express this concept through the types then we will do it.
    
    _Example:_
    ```haskell
    whenNotNull :: Applicative f => [a] -> (NonEmpty a -> f ()) -> f ()
    ```
    instead of
    ```haskell
    whenNotNull :: Applicative f => [a] -> ([a] -> f ()) -> f ()
    ```
3. **Performance.** Prefer `Text` over [`String`](https://www.reddit.com/r/haskell/comments/29jw0s/whats_wrong_with_string/),
   use spaceleak-free functions (like our custom `sum` and `product`), introduce
   `{-# INLINE #-}` and `{-# SPECIALIZE #-}` pragmas where appropriate.
4. **Minimalism** (low number of dependencies). We don't force users of `relude` to
   stick to some specific lens or text formatting or logging library.
5. **Convenience** (like lifted to `MonadIO` functions, more reexports). But we
   want to bring common types and functions (like `containers` and `bytestrng`)
   into scope because they are used in almost every application anyways.
6. **Provide excellent documentation.**
   * Tutorial
   * Migration guide from `Prelude`
   * Haddock with examples for (almost) every function
     (all examples are tested with [`doctest`](http://hackage.haskell.org/package/doctest))
   * Documentation regarding [internal module structure]((http://hackage.haskell.org/package/relude/docs/Relude.html))
   * `relude`-specific [HLint](http://hackage.haskell.org/package/hlint) rules:
     [`.hlint.yaml`](.hlint.yaml)
7. **User-friendliness.** Ability to quickly migrate to `relude` if you're familiar
   with the common libraries like `text` and `containers`.
8. **Exploration.** Experiment with new ideas and proposals without introducing
   breaking changes.

This README contains introduction to `Relude` and a tutorial on how to use it.

Structure of this tutorial
--------------------------

This tutorial has several parts:

1. [Motivation.](#motivation-)
2. [Get started.](#get-started-)
3. [Difference from `Prelude`.](#difference-from-prelude-)
4. [Reexports.](#reexports-)
5. [What's new?](#whats-new-)
6. [Migration guide.](#migration-guide-)

This is neither a tutorial on _Haskell_ nor tutorial on each function contained
in `Relude`. For detailed documentation of every function together with examples
and usage, see [_Haddock documentation_](http://hackage.haskell.org/package/relude).

Motivation [↑](#structure-of-this-tutorial)
------------------------------------------

We decided to base `relude` on `universum` due to the following reasons:

1. `universum` helps to achieve our goals more than any other custom prelude.
2. We worked on `universum` a lot (just check contributors statistics) and we
   know its internal structure.

The motivation to create another alternative prelude instead of modifying
existing one is that it's hard to change preludes in any way. `relude`
uses approach with `Extra.*` modules which are not exported by default so it's
quite easy to bring something new (that satisfies `relude` goals) and let users
decide to use it or not.

Unlike `universum`, we are:

1. Not trying to replace `Foldable` with custom `Container` type class. We only
   forbid `elem` and `notElem` functions for sets due to performance reasons.
2. Have less dependencies: no `vector`, no `microlens`, no `safe-exceptions`, no `type-operators`.
3. Have a lot of other different improvements.

Get started [↑](#structure-of-this-tutorial)
--------------------------------------------

If you want to start using `relude` in your project and explore it with the help
of compiler, set everything up according to the instructions below.

### `base-noprelude`

This is the recommended way to use custom prelude. It requires you to perform
the following steps:

1. Replace `base` dependency with corresponding version of `base-noprelude` in
   your `.cabal` file.
2. Add the following `Prelude` module to your project (both to filesystem and to `exposed-modules`):
   ```haskell
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

Difference from Prelude [↑](#structure-of-this-tutorial)
--------------------------------------------------------

* `head`, `tail`, `last`, `init` work with `NonEmpty a` instead of `[a]`.
* `undefined` triggers a compiler warning, because you probably don't want to
  leave `undefined` in your code. Either use `throwIO`, `Except`, `error` or
  `bug`.
* Multiple sorting functions are available without imports:
  + `sortBy :: (a -> a -> Ordering) -> [a] -> [a]`: sorts list using given custom comparator.
  + `sortWith :: Ord b => (a -> b) -> [a] -> [a]`: sorts a list based on some property of its elements.
  + `sortOn :: Ord b => (a -> b) -> [a] -> [a]`: just like `sortWith`, but more
    time-efficient if function is calculated slowly (though less
    space-efficient). So you should write `sortOn length` (would sort elements
    by length) but `sortWith fst` (would sort list of pairs by first element).
* Functions `sum` and `product` are strict now, which makes them more efficient.
* If you try to do something like `putStrLn "hi"`, you'll get an error message if
  `OverloadedStrings` is enabled – it happens because the compiler doesn't know what
  type to infer for the string. Use `putTextLn` in this case.
* Since `show` doesn't come from `Show` anymore, you need to export `Show` from
  `Text.Show` module if you want to implement `Show` instance manually.
* You can't call `elem` and `notElem` functions over `Set` and `HashSet`. These
  functions are forbidden for these two types because of the performance reasons.
* `error` takes `Text`.
* `lookup` doesn't work on list of pairs.

Reexports [↑](#structure-of-this-tutorial)
------------------------------------------

### Commonly used libraries

First of all, we reexport some generally useful modules: `Control.Applicative`,
`Data.Traversable`, `Data.Monoid`, `Control.DeepSeq`, `Data.List`, and lots of
others. Just remove unneeded imports after importing `Relude` (you can use
`.hlint.yaml` file for this).

Then, some commonly used types: `Map/HashMap/IntMap`, `Set/HashSet/IntSet`,
`Seq`, `Text` and `ByteString` (as well as synonyms `LText` and `LByteString`
for lazy versions).

`liftIO` and `MonadIO` are exported by default. A lot of `IO` functions are
generalized to `MonadIO`.

`deepseq` is exported. For instance, if you want to force deep evaluation of
some value (in IO), you can write `evaluateNF a`. WHNF evaluation is possible
with `evaluateWHNF a`.

We also reexport big chunks of these libraries: `mtl`, `stm`.

[`Bifunctor`](http://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Bifunctor.html)
type class with useful instances is exported.

* `first` and `second` functions apply a function to first/second part of a tuple (for tuples).
* `bimap` takes two functions and applies them to first and second parts respectively.

### Text

We export `Text` and `LText`, and some functions work with `Text` instead of `String` –
specifically, IO functions (`readFile`, `putStrLn`, etc) and `show`. In fact, `show`
is polymorphic and can produce strict or lazy `Text`, `String`, or `ByteString`.
Also, `toText/toLText/toString` can convert `Text|LText|String` types to
`Text/LText/String`. If you want to convert to and from `ByteString` use
`encodeUtf8/decodeUtf8` functions.

### Debugging and `undefined`s

`trace`, `traceM`, `traceShow`, etc. are available by default. GHC will warn you
if you accidentally leave them in code, however (same for `undefined`).

We also have `data Undefined = Undefined` (which, too, comes with warnings).

### Exceptions

TODO: write about reexports, `Bug` and `Exc` pattern.

What's new? [↑](#structure-of-this-tutorial)
--------------------------------------------

Finally, we can move to part describing the new cool features we bring with `relude`.

* Safe analogue for `head` function: `safeHead :: [a] -> Maybe a` or you can
  use our `viaNonEmpty` function to get `Maybe a`: `viaNonEmpty head :: [a] -> Maybe a`.
* `uncons` splits a list at the first element.
* `ordNub` and `sortNub` are _O(n log n)_ versions of `nub` (which is quadratic)
  and `hashNub` and `unstableNub` are almost _O(n)_ versions of `nub`.
* `(&)` – reverse application. `x & f & g` instead of `g $ f $ x` is useful sometimes.
* `whenM`, `unlessM`, `ifM`, `guardM` are available and do what you expect
  them to do (e.g. `whenM (doesFileExist "foo")`).
* General fold functions:
  ```haskell
  foldMapA :: (Monoid b, Applicative m, Foldable f) => (a -> m b) -> f a -> m b
  foldMapM :: (Monoid b, Monad m, Foldable f) => (a -> m b) -> f a -> m b
  ```
* `readMaybe` and `readEither` are like `read` but total and give either
  `Maybe` or `Either` with parse error.
* `when(Just|Nothing|Left|Right|NotEmpty)[M][_]`
  let you conditionally execute something. Before:

  ```haskell
  case mbX of
      Nothing -> return ()
      Just x  -> f x
  ```

  After:

  ```haskell
  whenJust mbX $ \x ->
      f x
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
* [`One` type class](src/Relude/Container/One.hs)
  for creating singleton containers. Even monomorhpic ones like `Text`.
* [`StaticMap` and `DynamicMap`type classes](src/Relude/Extra/Map.hs) as a
  general interface for `Map`-like data structures.
* `evaluateWHNF` and `evaluateNF` functions as clearer and lifted aliases for
  `evaluate` and `evaluate . force`.
* `MonadFail` instance for `Either`.

Migration guide [↑](#structure-of-this-tutorial)
------------------------------------------------

In order to replace default `Prelude` with `relude` you should start with instructions given in
[_get started_](#get-started-) section.

This section describes what you need to change to make your code compile with `relude`.

1. Enable `-XOverloadedStrings` extension by default for your project.
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
