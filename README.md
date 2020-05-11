# relude

![Logo](https://user-images.githubusercontent.com/8126674/67678250-9d8eab80-f99f-11e9-96ca-27883ceeefa6.png)

[![GitHub CI](https://github.com/kowainik/relude/workflows/CI/badge.svg)](https://github.com/kowainik/relude/actions)
[![Travis](https://img.shields.io/travis/kowainik/relude.svg?logo=travis)](http://travis-ci.org/kowainik/relude)
[![AppVeyor](https://ci.appveyor.com/api/projects/status/github/kowainik/relude?branch=master&svg=true)](https://ci.appveyor.com/project/kowainik/relude)
[![Hackage](https://img.shields.io/hackage/v/relude.svg?logo=haskell)](https://hackage.haskell.org/package/relude)
[![Stackage LTS](http://stackage.org/package/relude/badge/lts)](http://stackage.org/lts/package/relude)
[![Stackage Nightly](http://stackage.org/package/relude/badge/nightly)](http://stackage.org/nightly/package/relude)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**`relude`** is an alternative prelude library. If you find the default
`Prelude` unsatisfying, consider using `relude` instead.

## Key design principles

1. **Avoid all** [**partial functions**](https://www.reddit.com/r/haskell/comments/5n51u3/why_are_partial_functions_as_in_head_tail_bad/)
   (like `head :: [a] -> a`). The types of partial functions lie about their
   behavior and usage of such functions can lead to unexpected bugs. However,
   you can still use some unsafe functions from `Relude.Unsafe` module, but they
   are not exported by default.
2. **Type-safety**. Make invalid states unrepresentable. And if
   possible we do so through types.

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
   stick to some specific lens or text formatting or logging library. If
   possible, `relude` tries to depend only on boot libraries.
   The [Dependency graph](relude-dependency-graph.png) of `relude` can give you a clearer picture.
5. **Convenience** (e.g. functions lifted to `MonadIO`, more reexports). Despite minimalism, we
   want to bring common types and functions (like `containers` and `bytestring`)
   into scope because they are used in almost every application.
6. **Provide excellent documentation.**
   + Tutorial
   + Migration guide from `Prelude`
   + Haddock for every function with examples tested by [`doctest`](http://hackage.haskell.org/package/doctest)
   + Documentation on [internal module structure](http://hackage.haskell.org/package/relude/docs/Relude.html)
   + `relude`-specific [HLint](http://hackage.haskell.org/package/hlint) rules: [`.hlint.yaml`](.hlint.yaml)
7. **User-friendliness.** Ability to quickly migrate to `relude` if you're familiar
   with the common libraries like `text` and `containers`.
8. **Facilitate Exploration.** Experiment with new ideas and proposals without introducing
   breaking changes. `relude` uses the approach of having `Extra.*` modules which are not
   exported by default, making it quite easy to provide new functionality and let users
   decide to use it or not.

This README contains an introduction to `relude` and a tutorial.

For an introduction to alternative preludes, check the following
blog post by [Type Classes](https://typeclasses.com/) that highlights
`relude`.

* [No implicit Prelude](https://typeclasses.com/ghc/no-implicit-prelude)

## Structure of this tutorial

This tutorial has several parts:

1. [When to use an alternative prelude](#when-to-use-an-alternative-prelude-)
2. [Get started](#get-started-)
    * [Mixins](#mixins-)
    * [base-noprelude](#base-noprelude-)
    * [NoImplicitPrelude](#NoImplicitPrelude-)
3. [Difference from Prelude](#difference-from-prelude-)
4. [Reexports](#reexports-)
5. [What's new?](#whats-new-)
6. [Migration guide](#migration-guide-)
7. [Comparison with other alternative preludes](#comparison-with-other-alternative-preludes-)
    * [Relude vs Protolude](#relude-vs-protolude-)
8. [For developers](#for-developers-)

This is neither a tutorial on _Haskell_ nor a tutorial on each function contained
in `relude`. For detailed documentation of every function together with examples
and usage, see [_Haddock documentation_](http://hackage.haskell.org/package/relude).

## When to use an alternative prelude [↑](#structure-of-this-tutorial)

The module with the name `Prelude` is a module imported by default in every Haskell
source file of your project. If you want to use some data types or functions
which are not exposed by `Prelude`, you need to import them, adding necessary
libraries to your project dependencies. Unlike ordinary libraries, alternative
preludes make a different set of functions and data types available by default.

Replacing default `Prelude` from `base` has the following _disadvantages_:

1. Increased entry threshold: you need to learn a different standard library.
    + `relude` tries to keep this threshold as low as possible using excellent
      documentation, no custom abstractions, and changing behavior only for a small
      subset of functions.
2. Extra dependencies: adding more libraries to dependencies increases build
   times and maintenance burden.
   + `relude` depends only on boot libraries (almost) which results in small build time, follows
     [PVP](https://pvp.haskell.org/) and cares about backwards compatibility.

However, using an alternative prelude, specifically `relude`, has the following
**advantages**:

1. Increased code safety: no partial functions, no space-leak functions.
2. Increased productivity: no need to import common functions and data types,
   more common idioms provided.
3. Increased performance: some functions in `relude` are faster than in default `Prelude`.

Our recommendations when to use `relude`:

1. When you develop an application (e.g. CLI tool, web-app). In that case, greater
   productivity is more important than a low number of dependencies.
2. When writing a big framework. Some of them can be bigger than applications.

## Get started [↑](#structure-of-this-tutorial)

If you want to start using `relude` in your project and to explore it with the help
of the compiler, set everything up in one of the following ways:

### Mixins [↑](#structure-of-this-tutorial)

This is the recommended way to use a custom prelude. It requires you to perform
the following steps:

You can use the Cabal feature `mixins` to replace the default `Prelude` with `Relude`
without the need to add extra dependencies or to import `Relude` manually in each module.
See the following example:

> **NOTE:** this requires Cabal version to be at least `2.2`

```cabal
cabal-version:       2.2
name:                prelude-example
version:             0.0.0.0

library
  exposed-modules:     Example
  build-depends:       base >= 4.10 && < 4.13
                     , relude ^>= 0.6.0.0

  mixins:              base hiding (Prelude)
                     , relude (Relude as Prelude)

  default-language:    Haskell2010
```

> **NOTE:** if you use [`summoner`](https://github.com/kowainik/summoner) to generate your Haskell project,
> this tool can automatically create such a structure for you when you specify the custom prelude.

If you want to use a module that `relude` does not export by default
(e.g. `Relude.Extra.Enum`, `Relude.Unsafe`) , you need to list it
under the `mixins` field as well, like this:

```cabal
  mixins: base hiding (Prelude)
        , relude (Relude as Prelude, Relude.Extra.Enum)
```

If you want to import all `Extra.*` modules, you can add
just the `Relude.Extra` module to `mixins`, and later you can import all
extra functions and data types from `Relude.Extra`. This is the
easiest way to bring all safe functions from `relude` to your project.

```cabal
  mixins: base hiding (Prelude)
        , relude (Relude as Prelude, Relude.Extra)
```

### base-noprelude [↑](#structure-of-this-tutorial)

Alternatively, you can use the `base-noprelude` trick to use
alternative preludes. This approach can be useful if you want to have
your own `Prelude` module with some custom functions, not provided by
`relude`. To use the trick, perform the following steps:

1. Replace the `base` dependency with the corresponding version of `base-noprelude` in
   your `.cabal` file.
2. Add the `relude` dependency to your `.cabal` file.
3. Create the file called `Prelude.hs` in your source directory with
   the following content:
   ```haskell
   module Prelude
       ( module Relude
       ) where

   import Relude
   ```
4. Add this module to `exposed-modules` in your `.cabal` file:
   ```cabal
   exposed-modules: Prelude
   ```
5. Optionally modify your `Prelude` module to include more functions or less. 
   You may want to hide something from the `Relude` module. Or you may want to add
   something from the `Relude.Extra.*` modules!

This is a very convenient way to add a custom prelude to your project because
you don't need to both import a module manually and enable the
`NoImplicitPrelude` extension inside each `.hs` file.

### NoImplicitPrelude [↑](#structure-of-this-tutorial)

Disable the built-in prelude at the top of your file:

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
```

Or directly in your project `.cabal` file, if you want to use it in every module by
default:

```haskell
default-extensions: NoImplicitPrelude
```

Add `relude` as a dependency of your project. Then add the following import to
your modules:

```haskell
import Relude
```

## Difference from Prelude [↑](#structure-of-this-tutorial)

Main differences from `Prelude` can be grouped into the following categories:

* Changed behavior of common functions
  + `head`, `tail`, `last`, `init` work with `NonEmpty a` instead of `[a]`.
  * `lines`, `unlines`, `words`, `unwords` work with `Text` instead of `String`.
  + `show` is polymorphic over return type.
  + Functions `sum` and `product` are strict now, which makes them more efficient.
  + You can't call `elem` and `notElem` functions over `Set` and `HashSet`.
    These functions are forbidden for these two types for performance
    reasons.
  + `error` takes `Text`
  + `undefined` triggers a compiler warning, because you probably don't want to
    leave `undefined` in your code. Either use `throwIO`, `Except`, `error` or
    `bug`.
* Not reexported
  + `read`
  + `lookup` for lists
  + `log`
* Completely new functions are brought into scope
  + See [What's new?](#whats-new-) section for a detailed overview.
* New reexports
  + See [Reexports](#reexports-) section for a detailed overview.

## Reexports [↑](#structure-of-this-tutorial)

`relude` reexports some parts of the following libraries:

* [`base`](http://hackage.haskell.org/package/base)
* [`bytestring`](http://hackage.haskell.org/package/bytestring)
* [`containers`](http://hackage.haskell.org/package/containers)
* [`deepseq`](http://hackage.haskell.org/package/deepseq)
* [`hashable`](http://hackage.haskell.org/package/hashable)
* [`mtl`](http://hackage.haskell.org/package/mtl)
* [`stm`](http://hackage.haskell.org/package/stm)
* [`text`](http://hackage.haskell.org/package/text)
* [`transformers`](http://hackage.haskell.org/package/transformers)
* [`unordered-containers`](http://hackage.haskell.org/package/unordered-containers)

If you want to clean up imports after switching to `relude`, you can use
`relude`-specific [`.hlint.yaml`](.hlint.yaml) configuration for this task.

### base

Multiple sorting functions are available:
  + `sortBy :: (a -> a -> Ordering) -> [a] -> [a]`: sorts list using given custom comparator.
  + `sortWith :: Ord b => (a -> b) -> [a] -> [a]`: sorts a list based on some property of its elements.
  + `sortOn :: Ord b => (a -> b) -> [a] -> [a]`: just like `sortWith`, but more
    time-efficient and less space-efficient. So you should write `sortOn length` (would sort elements
    by length) but `sortWith fst` (would sort list of pairs by first element).

`readMaybe` and `readEither` are like `read` but  are total functions and give either `Maybe`
or `Either` with parse error.

`(&)` – reverse function application. The following three expressions are
semantically equivalent:

* `g (f x)`
* `g $ f $ x`
* `x & f & g`

Some generally useful modules from `base` package, like: `Control.Applicative`,
`Data.Traversable`, `Data.Monoid`, `Data.List`, and many others.

`liftIO` and `MonadIO` are exported by default. Many `IO` functions are
generalized to `MonadIO`.

[`Bifunctor`](http://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Bifunctor.html)
type class with useful instances is exported.

* `first` and `second` functions apply a function to first/second part of a tuple (for tuples).
* `bimap` takes two functions and applies them to first and second parts respectively.

`trace`, `traceM`, `traceShow`, etc. are available by default. However, GHC will warn you
if you accidentally leave them in code (just like for `undefined`).

We also have `data Undefined = Undefined` (which, too, comes with warnings).

`relude` reexports `Exception` type from the `base` package and introduces the
`bug` function as an alternative to `error`. There's also a very convenient
`Exc` pattern-synonym to handle exceptions of different types.

See [`Relude.Exception`](src/Relude/Exception.hs) module for details on exceptions.

### containers & unordered-containers

The following types from these two packages are exported:

* Maps: strict versions of `Map`, `HashMap`, `IntMap`.
* Sets: `Set`, `HashSet`, `IntSet`.
* Sequences: `Seq`.

### text & bytestring

`relude` exports `Text` and `ByteString` (as well as synonyms `LText`
and `LByteString` for lazy versions). In addition, some functions work
with `Text` instead of `String` – `words`, `lines`, etc. In
addition, `relude` provides specialised versions of the `IO` functions to
work with `Text` and `ByteString` — `readFileText`, `writeFileBS`,
etc.

`show` is polymorphic and can produce strict or lazy `Text` or
`ByteString` as well as `String`.

Also, `toText/toLText/toString` can convert between `Text|LText|String` types.
If you want to convert to and from `ByteString` use
`encodeUtf8/decodeUtf8` functions.

### transformers & mtl

The following parts of these two libraries are exported:

* Transformers: `State[T]`, `Reader[T]`, `ExceptT`, `MaybeT`.
* Classes: `MonadReader`, `MonadState`.

### Deepseq

`deepseq` is exported. For instance, if you want to force deep evaluation of
some value (in `IO`), you can write `evaluateNF a`. WHNF evaluation is possible
with `evaluateWHNF a`.

## What's new? [↑](#structure-of-this-tutorial)

Finally, we can start describing the cool new features `relude` brings us.

### Available by default

* Safe analogue for list functions: use `viaNonEmpty` function to get `Maybe a`.
  + `viaNonEmpty head :: [a] -> Maybe a`
* `uncons` splits a list at the first element.
* `ordNub` and `sortNub` are _O(n log n)_ versions of `nub` (which is quadratic)
  and `hashNub` and `unstableNub` are almost _O(n)_ versions of `nub`.
* `whenM`, `unlessM`, `ifM`, `guardM` are available and do what you expect
  them to do (e.g. `whenM (doesFileExist "foo")`).
* General fold functions:
  ```haskell
  foldMapA :: (Monoid b, Applicative m, Foldable f) => (a -> m b) -> f a -> m b
  foldMapM :: (Monoid b, Monad m, Foldable f) => (a -> m b) -> f a -> m b
  ```
* `when(Just|Nothing|Left|Right|NotEmpty)[M][_]`
  lets you conditionally execute something.

  **Before:**

  ```haskell
  case mbX of
      Nothing -> return ()
      Just x  -> f x
  ```

  **After:**

  ```haskell
  whenJust mbX $ \x ->
      f x
  ```

* `for_` for loops. There's also `forM_` but `for_` looks nicer.

  ```haskell
  for_ [1..10] $ \i -> do
      ...
  ```

* `andM`, `allM`, `anyM`, `orM` are monadic versions of the corresponding functions from `base`.
* Conversions between `Either` and `Maybe` like `rightToMaybe` and `maybeToLeft`
  with clear semantics.
* `using(Reader|State)[T]` functions as aliases for `flip run(Reader|State)[T]`.
* [`One` type class](src/Relude/Container/One.hs)
  for creating singleton containers. Even monomorphic ones like `Text`.
* `evaluateWHNF` and `evaluateNF` functions as clearer and lifted aliases for
  `evaluate` and `evaluate . force`.
* `MonadFail` instance for `Either`.

### Need to import explicitly

* Convenient functions to work with `(Bounded a, Enum a)` types:
  1. `universe :: (Bounded a, Enum a) => [a]`: get all values of the type.
  2. `inverseMap :: (Bounded a, Enum a, Ord k) => (a -> k) -> k -> Maybe a`: convert functions like `show` to parsers.

* Nice helpers to deal with `newtype`s in a more pleasant way:

  ```haskell
  ghci> newtype Foo = Foo Bool deriving Show
  ghci> under not (Foo True)
  Foo False
  ```

* Functions to operate with `CallStack`:

  ```haskell
  >>> foo :: HasCallStack => String; foo = ownName
  >>> foo
  "foo"
  ```

* `Foldable1` typeclass that contains generalized interface for folding
  non-empty structures like `NonEmpty`.
* `Validation` data type as an alternative to `Either` when you want to combine
  all errors.
* [`StaticMap` and `DynamicMap` type classes](src/Relude/Extra/Map.hs) as a
  general interface for `Map`-like data structures.

Explore `Extra` modules: [`Relude.Extra`](src/Relude/Extra/)

## Migration guide [↑](#structure-of-this-tutorial)

In order to replace default `Prelude` with `relude` you should start with the instructions given in the
section titled "[_get started_](#get-started-)".

### Code changes

This section describes what you need to change to make your code compile with `relude`.

1. Enable `-XOverloadedStrings` extension by default for your project.
2. Since `head`, `tail`, `last` and `init` work for `NonEmpty` you should
   refactor your code in one of the following three ways:
   1. Change `[a]` to `NonEmpty a` where it makes sense.
   2. Use functions which return `Maybe`. There is the `viaNonEmpty` function for this.
      And you can use it thus `viaNonEmpty last l`.
       + `tail` is `drop 1`. It's almost never a good idea to use `tail` from `Prelude`.
   3. Add `import qualified Relude.Unsafe as Unsafe` and replace function with qualified usage.
3. If you use `fromJust` or `!!` you should use import them from the `Unsafe` module thus:  `import qualified Relude.Unsafe as Unsafe`.
4. If you use `foldr` or `forM_` or similar for something like `Maybe a` or
   `Either a b` it's recommended to replace usages of such functions with
   monomorphic alternatives, such as:
   * `Maybe`
     + `(?:)          :: Maybe a -> a -> a`
     + `fromMaybe     :: a -> Maybe a -> a`
     + `maybeToList   :: Maybe a -> [a]`
     + `maybeToMonoid :: Monoid m => Maybe m -> m`
     + `maybe         :: b -> (a -> b) -> Maybe a -> b`
     + `whenJust      :: Applicative f => Maybe a -> (a -> f ()) -> f ()`
     + `whenJustM     :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()`

   * `Either`
     + `fromLeft    :: a -> Either a b -> a`
     + `fromRight   :: b -> Either a b -> b`
     + `either      :: (a -> c) -> (b -> c) -> Either a b -> c`
     + `whenRight_  :: Applicative f => Either l r -> (r -> f ()) -> f ()`
     + `whenRightM_ :: Monad m => m (Either l r) -> (r -> m ()) -> m ()`

5. Forget about `String` type.
   + Replace `(++)` with `(<>)` for `String`-like types.
   + Use `toText/toLText/toString` functions to convert to `Text/LazyText/String` types.
   + Use `encodeUtf8/decodeUtf8` to convert to/from `ByteString`.
   + Use `(putStr[Ln]|readFile|writeFile|appendFile)[Text|LText|BS|LBS]` functions.

6. Since `show` doesn't come from `Show` anymore, you need to export
  `Text.Show` module if you want to implement `Show` instance manually. This can be done like this:
  ```haskell
  import qualified Text.Show
  ```

7. Run `hlint` using `.hlint.yaml` file from `relude` package to cleanup code and imports.


### Running HLint on CI

Instead of storing a `relude`-specific `.hlint.yaml` file inside your repository,
you can run HLint with this file automatically on any CI service such as
[Travis CI](https://travis-ci.org/) or [Circle CI](https://circleci.com/).
For this you need to:

1. Find the commit hash of the `relude` version you are using (can be found in [releases](https://github.com/kowainik/relude/releases)).
2. Run the command that downloads `.hlint.yaml` for that version.
3. Run `hlint` using this file.

For the latest `relude` version, this can be achieved by executing the following
two commands on your CI:

```yaml
curl https://raw.githubusercontent.com/kowainik/relude/v0.6.0.0/.hlint.yaml -o .hlint-relude.yaml
curl -sSL https://raw.github.com/ndmitchell/neil/master/misc/travis.sh | sh -s -- hlint -h .hlint-relude.yaml .
```

See an example of this feature being used in [Summoner](https://github.com/kowainik/summoner/blob/b6c3ecb7cd9bc8d1451e2cc78cd020cd2e473564/.travis.yml#L58-L59).

## Comparison with other alternative preludes [↑](#structure-of-this-tutorial)

There are quite a few libraries that can be used as alternative preludes in
Haskell, let's compare Relude with some of them.

### Relude vs Protolude [↑](#structure-of-this-tutorial)

[Protolude](https://github.com/sdiehl/protolude) is one of the most popular
alternative preludes. It's also relatively small, but:

1. Protolude supports older GHC versions (from GHC 7.6.1) while `relude` only
   supports from GHC 8.0.2. So if you target ancient GHC versions, `protolude`
   might be a better choice. But because of that it contains a lot of CPP, and
   code is ugly in some places, as a consequence it's more difficult to modify.
2. `relude` has much better documentation:
    * [High-level overview of internal module structure](http://hackage.haskell.org/package/relude/docs/Relude.html)
    * 100% Haddock coverage
    * Almost every function has usage examples and all examples are tested with
      `doctest` (which is sometimes difficult to accomplish with multiple GHC
      versions, but we try really hard)
    * [Tutorial + migration guide](#structure-of-this-tutorial) from
      `Prelude` and just general description of the whole package and libraries
      it depends on.
3. `relude` has custom HLint rules specific to it: you can use them to remove
   redundant imports or find hints how to use functions from `relude`. Moreover,
   the HLint rules are generated using Dhall and there is [a blog post about
   this technique](https://kowainik.github.io/posts/2018-09-09-dhall-to-hlint).
   This makes maintaining HLint rules much easier.
4. `relude` has less dependencies and is slightly lighter because of that but 
   it is still very powerful and useful.
5. One minor difference: `head` in `protolude` returns `Maybe a` while in
   `relude` it works with `NonEmpty`.
6. Minor feature: `relude` uses type-level magic to forbid `elem` and `notElem`
   functions for `Set` and `HashSet` (because `elem` from `Foldable` runs in
   _O(n)_ time and you can accidentally use `elem` from `Foldable` but with
   `relude` you can't).
7. `relude` is opt-in oriented and has a notion of `Extra.*` modules that are
   not exported by default from the `Relude` module. So we don't pollute the
   global namespace but still have a lot of useful features like polymorphic 
   functions to work with every `newtype`, `Enum/Bounded`-related useful utilities,
   functions to take a name of any type as `Text` and much more. It's very easy
   to make them accessible package-wide with the `base-noprelude` trick!

## For Developers [↑](#structure-of-this-tutorial)

### Generating .hlint.yaml

Note, that we are using custom `hlint` settings which are `Relude` specific. To
keep it up to date don't forget to reflect your changes in this file. We are
using `Dhall` to maintain the configurations. To use it follow the steps below.

First time:

```shell
$ cabal new-install dhall-json
```

Dhall 9.0.0 is required, so make sure that the previous command installed
dhall-json >= 1.4.0.

To generate `hlint` file:

```shell
$ dhall-to-yaml --omitNull <<< './hlint/hlint.dhall' > .hlint.yaml
```

Check that you have generated a valid `.hlint.yaml` file without parse errors:

```shell
$ hlint test/Spec.hs
```

See our blog post where we describe the details of the implementation for this solution:

* [Dhall To HLint](https://kowainik.github.io/posts/2018-09-09-dhall-to-hlint)
