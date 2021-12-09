# relude

![Logo](https://user-images.githubusercontent.com/8126674/67678250-9d8eab80-f99f-11e9-96ca-27883ceeefa6.png)

[![GitHub CI](https://github.com/kowainik/relude/workflows/CI/badge.svg)](https://github.com/kowainik/relude/actions)
[![Hackage](https://img.shields.io/hackage/v/relude.svg?logo=haskell)](https://hackage.haskell.org/package/relude)
[![Stackage LTS](http://stackage.org/package/relude/badge/lts)](http://stackage.org/lts/package/relude)
[![Stackage Nightly](http://stackage.org/package/relude/badge/nightly)](http://stackage.org/nightly/package/relude)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**`relude`** is a safe, performant, user-friendly and lightweight Haskell
standard library.

The default `Prelude` is not perfect and doesn't always satisfies one's needs.
At this stage you may want to try an alternative prelude library. In this README
we are going to give you convincing reasons to consider using `relude` as such
alternative in your next project.

`relude` has some strong goals and principles that it sticks to. That principles
defines the library decisions and might tell you more about the priorities of
the library. So below you can find the key design principles behind `relude`:

1. **Productivity.** You can be more productive with a "non-standard" standard
   library, and `relude` helps you with writing safer and more
   efficient code faster.

2. **Total programming**. Usage of
   [_partial functions_](https://www.reddit.com/r/haskell/comments/5n51u3/why_are_partial_functions_as_in_head_tail_bad/)
   can lead to unexpected bugs and runtime exceptions in pure
   code. The types of partial functions lie about their behaviour. And
   even if it is not always possible to rely only on total functions,
   `relude` strives to encourage best-practices and reduce the
   chances of introducing a bug.

  | __Partial__                     | __Total__                                  |
  |---------------------------------|--------------------------------------------|
  | `head :: [a] -> a`              | `head :: NonEmpty a -> a`                  |
  | `tail :: [a] -> [a]`            | `tail :: NonEmpty a -> [a]`                |
  | `read :: Read a => String -> a` | `readMaybe :: Read a => String -> Maybe a` |
  | `fromJust :: Maybe a -> a`      | `fromMaybe :: a -> Maybe a -> a`           |

3. **Type-safety**. We use the *"make invalid states unrepresentable"* motto as one
   of our guiding principles. If it is possible, we express this concept through the
   types.

    _Example:_ Here the function's name type and actions are aligned with each other

    ```haskell
    whenNotNull :: Applicative f => [a] -> (NonEmpty a -> f ()) -> f ()
    ```
    While in here, the type could represent an unreachable state:

    ```haskell
    whenNotNull :: Applicative f => [a] -> ([a] -> f ()) -> f ()
    ```

4. **Performance.** We prefer `Text` over [`String`](https://www.reddit.com/r/haskell/comments/29jw0s/whats_wrong_with_string/),
   use space-leaks-free functions (e.g. our custom performant `sum` and `product`), introduce
   `{-# INLINE #-}` and `{-# SPECIALIZE #-}` pragmas where
   appropriate, and make efficient container types
   (e.g. `Map`, `HashMap`, `Set`) more accessible.

5. **Minimalism** (low number of dependencies). We do not force users of
   `relude` to stick to any specific lens or text formatting or logging
   library. Where possible, `relude` depends only on boot libraries.
   The [Dependency graph](https://raw.githubusercontent.com/kowainik/relude/main/relude-dependency-graph.png)
   of `relude` can give you a clearer picture.

6. **Convenience**. Despite minimalism, we want to bring commonly used
   types and functions into scope, and make available functions easier
   to use. Some examples of conveniences:

   + No need to add `containers`, `unordered-containers`, `text` and
     `bytestring` to dependencies in your `.cabal` file to use the
     main API of these libraries
   + No need to import types like `NonEmpty`, `Text`, `Set`, `Reader[T]`, `MVar`, `STM`
   + Functions like `liftIO`, `fromMaybe`, `sortWith` are available by default as well
   + `IO` actions are lifted to `MonadIO`

7. **Excellent documentation.**
   + Tutorial
   + Migration guide from `Prelude`
   + Haddock for every function with examples tested by
     [`doctest`](http://hackage.haskell.org/package/doctest)
   + Documentation on [internal module structure](http://hackage.haskell.org/package/relude/docs/Relude.html)
   + `relude`-specific [HLint](http://hackage.haskell.org/package/hlint) rules:
     [`.hlint.yaml`](https://github.com/kowainik/relude/blob/main/.hlint.yaml)

8. **User-friendliness.** Anyone should be able to quickly migrate to `relude`. Only
   some basic familiarity with the common libraries like `text` and `containers`
   should be enough (but not necessary).

9. **Exploration.** We have space to experiment with new ideas and proposals
   without introducing breaking changes. `relude` uses the approach with
   `Extra.*` modules which are not exported by default. The chosen approach makes it quite
   easy for us to provide new functionality without breaking anything and let
   the users decide to use it or not.

In addition to our key design principles, the following list of
**anti-goals** describes what `relude` is trying to avoid:

1. **Rewrite `base` from the ground up.** With `relude` you don't need
   to unlearn what you already knew, you can leverage existing
   knowledge to achieve higher productivity.
2. **Invent custom abstractions.** Learning abstractions is hard, so
   we do our best not to introduce new overwhelming concepts.
3. **Rename common definitions.** If something is called `foo` in
   `base`, it's also called `foo` in `relude`. So, `relude` doesn't
   rename any existing abstractions, but it may introduce a few new
   ones, if their benefits outweigh learning curve.

This README contains an introduction to `relude` and a tutorial on how to use it.

For a general introduction to alternative preludes, check the excellent
blog post by [Type Classes](https://typeclasses.com/) that highlights
`relude`.

* [No implicit Prelude](https://typeclasses.com/ghc/no-implicit-prelude)

## Structure of this tutorial

This tutorial has several parts:

1. [When to use an alternative prelude?](#when-to-use-an-alternative-prelude)
2. [Get started](#get-started)
    * [Mixins](#mixins)
    * [base-noprelude](#base-noprelude)
    * [NoImplicitPrelude](#NoImplicitPrelude)
3. [Difference from Prelude](#difference-from-prelude)
4. [Reexports](#reexports)
5. [What's new?](#whats-new)
6. [Migration guide](#migration-guide)
7. [Comparison with other alternative preludes](#comparison-with-other-alternative-preludes)
    * [Relude vs Protolude](#relude-vs-protolude)
8. [For developers](#for-developers)

This is neither a tutorial on _Haskell Standard Library_ nor a tutorial on each
function contained in `relude`. For latter see the detailed documentation of
every data type, type class and function together with examples and usages in
the [_Haddock documentation for `relude`_](http://hackage.haskell.org/package/relude).

## When to use an alternative prelude?

[[Back to the Table of Contents] ↑](#structure-of-this-tutorial)

The module with the name `Prelude` is a module imported by default in every Haskell
source file of your project. If you want to use some data types or functions
which are not exposed by `Prelude`, you need to import them, adding necessary
libraries to your project dependencies. Unlike ordinary libraries, alternative
preludes provide a different set of available by default functions and data
types by replacing the `Prelude` module.

Replacing default `Prelude` from `base` has the following _disadvantages_:

1. Increased entry threshold: you need to *learn* a different standard library.
    + `relude` tries to lower this threshold as much as possible: it comes with
      the excellent documentation, no custom abstractions, and behavior is
      changed only for a small subset of functions.
2. Extra dependencies: adding more libraries to dependencies increases build
   times and maintenance burden.
   + `relude` depends only on the boot libraries (almost) which results in small
     build time, follows [PVP](https://pvp.haskell.org/) and cares about
     backwards compatibility.

However, using an alternative prelude, specifically `relude`, has the following
**advantages**:

1. Increased code safety: no partial functions, no space-leak functions.
2. Increased productivity: no need to import common functions and data types,
   more common idioms are provided.
3. Increased performance: some functions in `relude` are faster than in the
   default `Prelude`.

Taking into consideration all the above points, we put together our
recommendations when to use `relude`:

1. When you develop an application (e.g. CLI tool, web-app). In that case,
   greater productivity is more important than a low number of dependencies.
2. When writing a big framework. Some of them can be bigger than applications.

And when you may want to stay with the default standard:

1. When you write a small library that is supposed to be used by other people in
   their projects.

## Get started

[[Back to the Table of Contents] ↑](#structure-of-this-tutorial)

If you want to start using `relude` in your project, you can set the library up
for you by one of the following ways.

### Mixins

[[Back to the Table of Contents] ↑](#structure-of-this-tutorial)

This is the recommended way to use a custom prelude.

You can use the Cabal feature `mixins` to replace the default `Prelude` with
`Relude` without the need to add extra dependencies or to import `Relude`
manually in each module.
For this you need to add the following lines into your `.cabal` file:

```haskell
  mixins:   base hiding (Prelude)
          , relude (Relude as Prelude)
          , relude
```

> **NOTE:** this requires Cabal version to be at least `2.2`

The above syntax does the following:

1. Makes all modules of `base` available except `Prelude`.
2. Renames the `Relude` module in `relude` to `Prelude`.
3. Additionally allows importing all other modules from `relude`
   (extra modules and reexports from other libraries).

See the following complete example of how your `.cabal` file may look
like after the set up:

```cabal
cabal-version:       2.2
name:                prelude-example
version:             0.0.0.0

library
  exposed-modules:     Example
  build-depends:       base >= 4.10 && < 4.13
                     , relude ^>= 1.0.0.0

  mixins:              base hiding (Prelude)
                     , relude (Relude as Prelude)
                     , relude

  default-language:    Haskell2010
```

> **NOTE:** if you use [`summoner`](https://github.com/kowainik/summoner) to
> generate a Haskell project, the tool automatically creates the `mixins`
> field when you specify a custom prelude.

If you want to restrict allowed modules in `relude` to a specific list
(e.g. use only `Relude.Extra.Enum` or `Relude.Unsafe` or `Data.Text`
from `text`), you can alternatively list them explicitly under the
first `mixins` entry field as well, like this:

```cabal
  mixins: base hiding (Prelude)
        , relude (Relude as Prelude
                 , Relude.Extra.Enum
                 , Relude.Unsafe
                 , Data.Text
                 )
```

If you want to bring only all `Extra.*` modules into scope, you can add
a single `Relude.Extra` module to `mixins`, and after that you can import all
extra functions and data types from `Relude.Extra`. This is the
easiest way to bring all functions and types from `relude` to your project
(excluding `Relude.Unsafe`).

```cabal
  mixins: base hiding (Prelude)
        , relude (Relude as Prelude
                 , Relude.Extra
                 )
```

> **NOTE:** due to the existing [stack issue](https://github.com/commercialhaskell/stack/issues/5077),
> `mixins` technique doesn't work with the `stack repl` at the moment. Please,
> consider this before using this method of turning `relude` on in the project.
>
> If having `stack repl` crucial for your workflow, see the following options of
> how to use Relude in your project.

### base-noprelude

[[Back to the Table of Contents] ↑](#structure-of-this-tutorial)

Alternatively, you can use the `base-noprelude` trick to enable
alternative preludes. This approach can be helpful if you want to have
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
5. Optionally modify your `Prelude` module to include more or fewer functions.
   Potentially, you can hide something from the `Relude` module. Or maybe you
   want to add something from `Relude.Extra.*` modules!
   Customize the module for your needs.

This is a very convenient way to add a custom prelude to your project because
you don't need to import module manually inside each file and enable the
`NoImplicitPrelude` extension.

### NoImplicitPrelude

[[Back to the Table of Contents] ↑](#structure-of-this-tutorial)

For this option, you need to disable the default `Prelude` module first.
To disable the built-in prelude on module basis, you can add the following
pragma at the top of your file:

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
```

if you want to disable the default `Prelude` for every module by default, you
can specify this directly in your project `.cabal` file:

```haskell
default-extensions: NoImplicitPrelude
```

Then you need to add `relude` as a dependency of your project.

After doing all above, you can now use `Relude` in any module of your project by
adding a single import:

```haskell
import Relude
```

## Difference from Prelude

[[Back to the Table of Contents] ↑](#structure-of-this-tutorial)

Main differences from `Prelude` can be grouped into the following categories:

* Changed behavior of common functions
  + `head`, `tail`, `last`, `init` work with `NonEmpty a` instead of `[a]`.
  * `lines`, `unlines`, `words`, `unwords` work with `Text` instead of `String`.
  + `show` is polymorphic over the return type.
  + Functions `sum` and `product` are strict now, which makes them more efficient.
  + You can't call `elem` and `notElem` functions over `Set` and `HashSet`.
    These functions are forbidden for these two types due to performance reasons.
  + `error` takes `Text`.
  + `undefined` triggers a compiler warning, because you probably don't want to
    leave `undefined` in your code. Either use `throwIO`, `Except`, `error` or
    `bug`.
* Not reexported
  + `read`
  + `lookup` for lists
  + `log`
* Completely new functions are brought into scope
  + See the [What's new?](#whats-new) section for a detailed overview.
* New reexports
  + See the [Reexports](#reexports) section for a detailed overview.

## Reexports

[[Back to the Table of Contents] ↑](#structure-of-this-tutorial)

`relude` reexports some parts of the following libraries:

* [`base`](http://hackage.haskell.org/package/base)
* [`bytestring`](http://hackage.haskell.org/package/bytestring)
* [`containers`](http://hackage.haskell.org/package/containers)
* [`deepseq`](http://hackage.haskell.org/package/deepseq)
* [`ghc-prim`](http://hackage.haskell.org/package/ghc-prim)
* [`hashable`](http://hackage.haskell.org/package/hashable)
* [`mtl`](http://hackage.haskell.org/package/mtl)
* [`stm`](http://hackage.haskell.org/package/stm)
* [`text`](http://hackage.haskell.org/package/text)
* [`transformers`](http://hackage.haskell.org/package/transformers)
* [`unordered-containers`](http://hackage.haskell.org/package/unordered-containers)

If you want to clean up your imports after switching to `relude`, you can use
the `relude`-specific
[`.hlint.yaml`](https://github.com/kowainik/relude/blob/main/.hlint.yaml)
configuration for this task. With this config, `HLint` will produce
warnings and hints on how to have more benefits from `relude`.

### base

Multiple sorting functions are available for different use-cases:

  + `sortBy :: (a -> a -> Ordering) -> [a] -> [a]`: sorts a list using given
    custom comparator.
  + `sortWith :: Ord b => (a -> b) -> [a] -> [a]`: sorts a list based on some
    property of its elements.
  + `sortOn :: Ord b => (a -> b) -> [a] -> [a]`: similar to `sortWith`, but more
    time-efficient if function is calculated slowly (though less
    space-efficient). So you should write `sortOn length` (would sort elements
    by length) but `sortWith fst` (would sort list of pairs by first element).

`readMaybe` and `readEither` are similar to `read` but unlike it, they are total
and return either `Maybe` or `Either` with a parse error.

`(&)` is the reverse application. The following three expressions are
semantically equivalent:

* `g (f x)`
* `g $ f $ x`
* `x & f & g`

Some generally useful modules from `base` package are exported, e.g.
`Control.Applicative`, `Data.Traversable`, `Data.Monoid`, `Data.List`,
and many more.

`liftIO` and `MonadIO` are exported by default. A lot of `IO` functions are
generalized to `MonadIO`.

[`Bifunctor`](http://hackage.haskell.org/package/base/docs/Data-Bifunctor.html)
type class with useful instances is exported.

* `first` and `second` functions apply a function to the first and
  second part of a `Bifunctor` (`fst` and `snd` for tuples, `Left` and
  `Right` for `Either`).
* `bimap` takes two functions and applies them to the first and second parts respectively.

`trace`, `traceM`, `traceShow`, etc. are available by default. However, GHC will
warn you if you accidentally leave them in code. Same goes for the `undefined`
function.

We also have `data Undefined = Undefined` (which also comes with the warning).

`relude` reexports `Exception` type from the `base` package and introduces the
`bug` function as an alternative to `error`. There is also a very convenient
`Exc` pattern-synonym to handle exceptions of different types.

See
[`Relude.Exception`](http://hackage.haskell.org/package/relude/docs/Relude-Exception.html)
module for details on exceptions.

### containers & unordered-containers

The following types from these two packages are exported:

* Maps: strict versions of `Map`, `HashMap`, `IntMap`.
* Sets: `Set`, `HashSet`, `IntSet`.
* Sequences: `Seq`.

### text & bytestring

`relude` exports `Text` and `ByteString` (as well as their lazy versions —
`LText` and `LByteString`).

Also, some functions now work
with `Text` instead of `String` – `words`, `lines`, etc.

In addition, `relude` provides specialised versions of the `IO` functions to
work with `Text` and `ByteString` — `readFileText`, `writeFileBS`, etc.

`show` is polymorphic and can produce strict or lazy `Text` or
`ByteString` as well as `String`.

Also, `toText|toLText|toString` can convert `Text|LText|String` types to
`Text|LText|String`. If you want to convert to and from `ByteString` use
`encodeUtf8|decodeUtf8` functions.

### transformers & mtl

The following parts of these two libraries are exported:

* Transformers: `State[T]`, `Reader[T]`, `ExceptT`, `MaybeT`.
* Classes: `MonadReader`, `MonadState`.

### Deepseq

All the main parts of the `deepseq` library are exported.
For instance, if you want to force the deep evaluation of
some value (in `IO`), you can write `evaluateNF a`.
Additionally, the WHNF evaluation is possible
with provided `evaluateWHNF`.

## What's new?

[[Back to the Table of Contents] ↑](#structure-of-this-tutorial)

Finally, let's move to part describing the new cool features we bring with
`relude`.

### Available by default

* Safe analogue for the list functions:
  ```haskell
  head :: NonEmpty a -> a    -- the first element of the list
  tail :: NonEmpty a -> [a]  -- the list without the first element
  last :: NonEmpty a -> a    -- the last element of the list
  init :: NonEmpty a -> [a]  -- the list without the last element
  ```

  You can also still work with lists for these functions. Using `viaNonEmpty`
  function you will get `Maybe a` from the list:

  ```haskell
  -- viaNonEmpty head :: [a] -> Maybe a
  ghci> viaNonEmpty head [1,2,3]
  Just 1
  ghci> viaNonEmpty head []
  Nothing
  ```

* `uncons` splits a list at the first element.
* `ordNub` and `sortNub` are _O(n log n)_ versions of `nub` (which is quadratic),
  also, `hashNub` and `unstableNub` are almost _O(n)_ versions of `nub`,
  and `intNub` for fast `Int`s nub.
* `whenM`, `unlessM`, `ifM`, `guardM` — monadic guard combinators, that work
  with any `Monad`, e.g. `whenM (doesFileExist "foo")`.
* General fold functions:
  ```haskell
  foldMapA :: (Monoid b, Applicative m, Foldable f) => (a -> m b) -> f a -> m b
  foldMapM :: (Monoid b, Monad m, Foldable f) => (a -> m b) -> f a -> m b
  ```
* `when(Just|Nothing|Left|Right|NotEmpty)[M][_]` functions that
  let you conditionally execute something.

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

* `for_` and `forM_` for loops.

  ```haskell
  for_ files $ \file -> do
      ...
  ```

* `andM`, `allM`, `anyM`, `orM` are monadic versions of the corresponding
  functions from `base`.
* Conversions between `Either` and `Maybe`, e.g. `rightToMaybe` and `maybeToLeft`
  with clear semantics.
* `using(Reader|State)[T]` functions as aliases for `flip run(Reader|State)[T]`.
* [`One` type class](http://hackage.haskell.org/package/relude/docs/Relude-Container-One.html)
  for creating singleton containers (even monomorphic ones like `Text`).
* `evaluateWHNF` and `evaluateNF` functions as clearer and lifted aliases for
  `evaluate` and `evaluate . force`.
* `MonadFail` instance for `Either`.

### Extra bonuses

`relude` has a number of `Extra` modules that are not exposed by default (they
are not a part of the `Relude` module). You need to import such modules
separately.

These extra modules include the following functionality:

* Convenient functions to work with `(Bounded a, Enum a)` types:
  1. `universe :: (Bounded a, Enum a) => [a]`: get all values of the type.

     ```haskell
     ghci> universe @Bool
     [True,False]
     ```
  2. `inverseMap :: (Bounded a, Enum a, Ord k) => (a -> k) -> k -> Maybe a`:
     convert functions like `show` to parsers:

     ```haskell
     readMyEnums :: Text -> Maybe MyEnum
     readMyEnums = inverseMap myShow
     ```

* Nice helpers to deal with `newtype`s in a more pleasant way:

  ```haskell
  ghci> newtype Foo = Foo Bool deriving Show
  ghci> under not (Foo True)
  Foo False
  ```

* Functions to operate with `CallStack`:

  ```haskell
  ghci> foo :: HasCallStack => String; foo = ownName
  ghci> foo
  "foo"
  ```

* `Foldable1` typeclass that contains generalized interface for folding
  non-empty structures like `NonEmpty`.
* [`StaticMap` and `DynamicMap` type classes](https://github.com/kowainik/relude/blob/main/src/Relude/Extra/Map.hs) as a
  general interface for `Map`-like data structures.
* And much more!

Explore `Extra` modules: [`Relude.Extra`](http://hackage.haskell.org/package/relude/docs/Relude-Extra.html)

## Migration guide

[[Back to the Table of Contents] ↑](#structure-of-this-tutorial)

This section of the guide helps to migrate your project from `base`
to the `relude` library.

In order to replace the default `Prelude` with `relude` you should start with
instructions provided in the  [_Get Started_](#get-started) section.

### Code changes

This section describes what you need to change to make your code compile with `relude`.

1. Enable `-XOverloadedStrings` extension by default for your project.
2. Since `head`, `tail`, `last` and `init` work for `NonEmpty` you should
   refactor your code in one of the described below ways:
   1. Change `[a]` to `NonEmpty a` where it makes sense.
   2. Use functions which return `Maybe`. There is the `viaNonEmpty` function for this.
      And you can use it like `viaNonEmpty last l`.
       + `tail` is `drop 1`. It's almost never a good idea to use `tail` from `Prelude`.
   3. Add `import qualified Relude.Unsafe as Unsafe` and replace the function
      with its qualified usage: `Unsafe.head`.
3. If you use `fromJust` or `!!` you should import them from `import qualified Relude.Unsafe as Unsafe`.
4. If you use `foldr` or `forM_` or similar for something like `Maybe a` or
   `Either a b` it is recommended to replace usages of such functions with the
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
     + `fromLeft    :: a -> Either a b -> a`
     + `fromRight   :: b -> Either a b -> b`
     + `either      :: (a -> c) -> (b -> c) -> Either a b -> c`
     + `whenRight_  :: Applicative f => Either l r -> (r -> f ()) -> f ()`
     + `whenRightM_ :: Monad m => m (Either l r) -> (r -> m ()) -> m ()`

5. Replace the `String` type with more efficient and suitable ones (e.g. `Text`):
   + Replace `(++)` with `(<>)` for `String`-like types.
   + Use `toText/toLText/toString` functions to convert to `Text/LazyText/String` types.
   + Use `encodeUtf8/decodeUtf8` to convert to/from `ByteString`.
   + Use `(putStr[Ln]|readFile|writeFile|appendFile)[Text|LText|BS|LBS]` functions.

6. Since `show` doesn't come from `Show` anymore, you need to import the
  `Text.Show` module if you want to implement `Show` instance manually. This can be done in the following way:

  ```haskell
  import qualified Text.Show

  data MyType = ...

  instance Show MyType where
      show :: MyType -> String
      show = ...
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
curl https://raw.githubusercontent.com/kowainik/relude/v1.0.0.0/.hlint.yaml -o .hlint-relude.yaml
curl -sSL https://raw.github.com/ndmitchell/neil/master/misc/travis.sh | sh -s -- hlint -h .hlint-relude.yaml .
```

See an example of this feature described in the following blog post
about Travis CI settings:

* [Kodimensional: Dead simple Haskell Travis settings for cabal and stack](https://kodimensional.dev/posts/2019-02-25-haskell-travis#customization-hlint)

## Comparison with other alternative preludes

[[Back to the Table of Contents] ↑](#structure-of-this-tutorial)

There are quite a few libraries that can be used as alternative preludes in
Haskell, let's compare Relude with some of them.

### Relude vs Protolude

[[Back to the Table of Contents] ↑](#structure-of-this-tutorial)

[Protolude](https://github.com/sdiehl/protolude) is one of the most popular
alternative preludes. It's also relatively small, but:

1. `relude` has custom HLint rules specific to it: you can use them to remove
   redundant imports or find hints on how to use functions from `relude`. Moreover,
   the HLint rules are generated using Dhall and there is [a blog post about
   this technique](https://kowainik.github.io/posts/2018-09-09-dhall-to-hlint).
   This allows to maintain HLint rules with less effort, even though it is
   already not an easy task.
2. One significant difference: `head` in `protolude` returns `Maybe a` while in
   `relude` it works with `NonEmpty`.
3. `relude` uses type-level features to provide better error messages
   on the difference from `Prelude`, and also to forbid `elem` and `notElem`
   functions for `Set` and `HashSet` (because `elem` from `Foldable` runs in
   _O(n)_ time and you can accidentally use `elem` from `Foldable` but with
   `relude` you can't).
4. Protolude supports older GHC versions (from GHC 7.6.1) while `relude` only
   supports from GHC 8.2.2. So if you aim ancient GHC versions, `protolude`
   might be a better choice. But because of that it contains a lot of CPP, code
   is scary in some places as a consequence and it is more difficult to add,
   remove or change things there.
5. `relude` has much better documentation:
    * [High-level overview of internal module structure](http://hackage.haskell.org/package/relude/docs/Relude.html)
    * 100% Haddock coverage
    * Every function has usage examples and all examples are tested with
      `doctest` (which also sometimes hard to do due to the multiple GHC
      versions support, but we try really hard)
    * [Tutorial + migration guide](#structure-of-this-tutorial) from
      `Prelude` and just general description of the whole package and libraries
      it depends on.
6. `relude` has fewer dependencies and is slightly lighter because of that but still
   is very powerful and useful.
7. `relude` is opt-in oriented and has a notion of `Extra.*` modules that are
   not exported by default from the `Relude` module. That means that we do not
   spoil the global namespace but still have a lot of useful features, like
   polymorphic functions to work with every `newtype`, `Enum/Bounded`-related
   useful utilities, functions to take a name of any type as `Text` and much
   more. It is a straightforward process to make them accessible package-wide
   with the `base-noprelude` trick!

## For Developers

[[Back to the Table of Contents] ↑](#structure-of-this-tutorial)

### Generating .hlint.yaml

Note, that we are using custom `hlint` setting which are `Relude` specific. To
keep it up to date don't forget to reflect your changes in this file. We are
using `Dhall` to maintain the configurations. To use it follow the steps below.

First time:

```shell
$ cabal v2-install dhall-yaml
```

Dhall 16.0.0 is required, so make sure that the previous command installed
`dhall-yaml` >= 1.2.5.

To generate `hlint` file:

```shell
$ dhall-to-yaml-ng <<< './hlint/hlint.dhall' > .hlint.yaml
```

Check that you have generated valid `.hlint.yaml` file without parse errors:

```shell
$ hlint test/Spec.hs
```

See our blog post where we describe the details of the implementation for this solution:

* [Dhall To HLint](https://kowainik.github.io/posts/2018-09-09-dhall-to-hlint)

### Producing dependency graph

Install `cabal-plan` first:

```shell
$ cabal v2-install cabal-plan
$ cabal-plan --version
cabal-plan 0.6.2.0
```

Then draw the graph only for the library dependencies:

```shell
cabal-plan dot --root lib:relude | dot -Tpng -o relude-dependency-graph.png
```
