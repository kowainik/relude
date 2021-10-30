# Changelog

`relude` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

## 1.1.0.0

* Remove Option from Data.Semigroup (which was removed from base in base 4.16)

## 1.0.0.1 — Mar 15, 2021

* Minor documentation changes.

## 1.0.0.0 — Mar 12, 2021

* [#353](https://github.com/kowainik/relude/issues/353):
  Reexport most common modules from the following libraries:

    + `containers`
    + `unordered-containers`
    + `text`
    + `bytestring`

  Now, when using `relude`, you don't need to add these libraries to
  your `.cabal` file to enjoy their main API. Try removing them from
  your `.cabal` file after upgrading to this version of `relude` to
  see if you still need them.

  To utilise this feature, update the `mixin` part of your package
  configuration (if you're using the mixins approach) to the following:

  ```cabal
  mixins:   base hiding (Prelude)
          , relude (Relude as Prelude)
          , relude
  ```

* [#345](https://github.com/kowainik/relude/issues/345):
  Support GHC-9.0.
* Upgrade minor GHC versions to GHC-8.10.4 and GHC-8.8.4.
* [#268](https://github.com/kowainik/relude/issues/268):
  Drop support of GHC-8.0.2.
* [#270](https://github.com/kowainik/relude/issues/270):
  Standardise `universe`, `universeNonEmpty` and `inverseMap` functions that
  previously were introduced in the `Relude.Extra.Enum` module. `Relude.Enum`
  module created that is exported in the main `Relude` module by default.

  __Migration guide:__ If you were using any of these functions you can now
  remove `Relude.Extra.Enum` from your imports and explicit `mixins` section
  as they are available for you with the `Relude` module.
* Remove the `Eq` constraint on `universeNonEmpty`
* [#269](https://github.com/kowainik/relude/issues/269):
  Remove the `Relude.Extra.Validation` module.

  __Migration guide:__
  If you use `Relude.Extra.Validation` in you project you need to:

    1. Add `validation-selective` into the `build-depends` section of your
       `.cabal` file.
    2. Change imports of `Relude.Extra.Validation` to `Validation`:

       ```haskell
       -- Was:
       import Relude.Extra.Validation (Validation (..), ..)
       -- Became:
       import Validation (Validation (..), ..)
       ```

* [#346](https://github.com/kowainik/relude/issues/346),
  [#347](https://github.com/kowainik/relude/issues/347):
  Reimplement `ordNub` through `nubOrd` from `containers`.
  Add `ordNubOn`, `intNub` and `intNubOn` functions.
* [#327](https://github.com/kowainik/relude/issues/327):
  Add `infinitely` as more strictly typed `forever`.
* [#311](https://github.com/kowainik/relude/issues/311):
  Add `maybeAt` function — the non-operator version of `!!?` with its
  arguments flipped.
* [#314](https://github.com/kowainik/relude/issues/314):
  Add lifted versions of functions to work with `Handle`:

    + `hFlush`
    + `hIsEOF`
    + `hSetBuffering`
    + `hGetBuffering`
* [#305](https://github.com/kowainik/relude/issues/305):
  Add lifted versions of functions to work with environment:

    + `getArgs`
    + `lookupEnv`
* Add lifted version of the `readFile'` function.
* Reexport the `BufferMode` type from `base`.
* [#309](https://github.com/kowainik/relude/issues/309):
  Reexport `span` from `Data.List`.
* [#319](https://github.com/kowainik/relude/issues/319):
  Implement `partitionWith`.
* [#307](https://github.com/kowainik/relude/issues/307):
  Add `foldr1` to `Foldable1`.
* [#316](https://github.com/kowainik/relude/issues/316):
  Add `average` and `average1` — efficient functions for finding
  average on foldable structures.
* [#306](https://github.com/kowainik/relude/issues/306):
  Add `maximumOn1` and `minimumOn1` to `Foldable1`.
* [#301](https://github.com/kowainik/relude/issues/301):
  Add `traceShowWith` to `Relude.Debug`.
* [#304](https://github.com/kowainik/relude/issues/304),
  [#317](https://github.com/kowainik/relude/issues/317):
  Various documentation improvements.
* Updates to `relude`-specific `.hlint` rules.

Thanks @googleson78, @sushi-shi, @rektrex, @aleator, @mjgpy3, @dalpd,
@Bodigrim for helping with this release!

## 0.7.0.0 — May 14, 2020

* [#253](https://github.com/kowainik/relude/issues/253):
  Support GHC-8.10. Upgrade GHC-8.8 to 8.8.3.
* Significant documentation improvements:
    + Add high-level description to each reexported module.
    + Add String Conversion Table.
    + Add `NonEmpty` lists functions tables.
    + Add `@since` annotations.
    + Improve README.
    + Inline some external reexports into explicit lists of exports.
    + Rewrite top-level `cabal` description.
* [#234](https://github.com/kowainik/relude/issues/234):
  Reexport `scanl1`, `scanr1`, `scanl'` from `Data.List`.
* [#256](https://github.com/kowainik/relude/issues/256):
  Make `cycle` total function.
* [#233](https://github.com/kowainik/relude/issues/233):
  Add `etaReaderT` to `Relude.Monad.Trans` to help with performance.
* [#294](https://github.com/kowainik/relude/issues/294):
  Add `atomicModifyIORef_` and `atomicModifyIORef'_`.
* [#293](https://github.com/kowainik/relude/issues/293):
  Add `memptyIfFalse` and `memptyIfTrue` functions.
* Reexport `NonEmpty` functions from `Relude.List.NonEmpty` instead of
  `Relude.List.Reexport`.
* [#239](https://github.com/kowainik/relude/issues/239):
  Reexport more STM functions that work with `TMVar` from
  `Relude.Lifted.Concurrent`.
* [#227](https://github.com/kowainik/relude/issues/227):
  Create `Relude.Extra` module
* [#228](https://github.com/kowainik/relude/issues/228):
  Add `universeNonEmpty` function.
* [#249](https://github.com/kowainik/relude/issues/249):
  __Breaking change:__ Fix infix of the `Relude.Extra.Lens` `(^.)` operator.
  Change it to `infixl 8`.
* Reexport partial `read` from `Relude.Unsafe` for consistency.
* [#244](https://github.com/kowainik/relude/issues/244):
  Remove deprecated functions: `prec`, `dupe` and `mapBoth`.

  __Migration rules:__
    + `prec`: use `prev` instead
    + `dupe`: use `dup` instead
    + `mapBoth`: use `bimapBoth` instead

* [#246](https://github.com/kowainik/relude/issues/246):
  Deprecate `Relude.Extra.Validation` in favour of
  [`validation-selective`](https://hackage.haskell.org/package/validation-selective)

  __Migration rules:__
  If you use `Relude.Extra.Validation` in you project you need to:

    1. Add `validation-selective` into the `build-depends` section of your
       `.cabal` file.
    2. Change imports of `Relude.Extra.Validation` to `Validation`:

       ```haskell
       -- Was:
       import Relude.Extra.Validation (Validation (..), ..)
       -- Became:
       import Validation (Validation (..), ..)
       ```
* [#196](https://github.com/kowainik/relude/issues/196):
  Deprecate `mapToFst` and `mapToSnd`. Introduce `toFst` and `toSnd`
  in `Relude.Extra.Tuple` as shorter aliases for `mapToFst`. Implement
  `fmapToFst` and `fmapToSnd`. Add more HLint rules for
  `Relude.Extra.Tuple` functions.

  __Migration rules:__

  + Replace `mapToFst` with `toFst`
  + Replace `mapToSnd` with `toSnd`
  + You can now use `fmapToFst` and `fmapToSnd` instead of
    `[f]map (mapToFst f)` and `[f]map (mapToSnd f)`
* [#286](https://github.com/kowainik/relude/issues/286):
  __Breaking change:__ `readEither` is not polymorphic over the first argument
  anymore. Now it takes `String`.

  __Migration rules:__ Use one of the conversion function from the
  `Relude.String.Conversion` module to covert your old input value into
  `String`.

  For example, if you had

  ```haskell
  readEither @Text @Int myText
  ```

  Now it should become:

  ```haskell
  readEither @Int (toString myText)
  ```

* [#281](https://github.com/kowainik/relude/issues/281):
  Move `One` property tests from `doctest` to `hedgehog`.
  Significant test time boost.
* [#264](https://github.com/kowainik/relude/issues/264):
  Support Dhall-16.0.0 in HLint rules.

## 0.6.0.0 — Oct 30, 2019

* [#171](https://github.com/kowainik/relude/issues/171):
  Add custom type errors to various functions and instances.
  + `head`, `tail`, `last`, `init`
  + `words`, `unwords`, `lines`, `unlines`
  + `error`
  + `ToText`, `ToLText`, `ToString` instances for bytestrings
  + `Foldable1` instance for ordinary lists
  + `Monad` instance for `Validation`

  (by [@vrom911](https://github.com/vrom911), [@chshersh](https://github.com/chshersh))
* [#164](https://github.com/kowainik/relude/issues/164):
  Reexport `ShortByteString`, `toShort`/`fromShort` functions.
  (by [@vrom911](https://github.com/vrom911))
* [#182](https://github.com/kowainik/relude/issues/182):
  Support GHC-8.8.1.
  (by [@chshersh](https://github.com/chshersh))
* [#168](https://github.com/kowainik/relude/pull/168),
  [#197](https://github.com/kowainik/relude/pull/197):
  Improve documentation significantly (more and better examples, better wording).
  (by [@chshersh](https://github.com/chshersh),
  [@vrom911](https://github.com/vrom911),
  [@Cmdv](https://github.com/Cmdv))
* [#177](https://github.com/kowainik/relude/issues/177):
  Improve usage of performance pragmas.
  (by [@chshersh](https://github.com/chshersh))
* [#167](https://github.com/kowainik/relude/issues/167):
  Rename functions (and deprecate old versions):
    + `prec` to `prev`
    + `dupe` to `dup`

  (by [@Cmdv](https://github.com/Cmdv), [@chshersh](https://github.com/chshersh))
* [#192](https://github.com/kowainik/relude/issues/192):
  Reexport `foldMap'` from `Data.Foldable`.
  (by [@tfausak](https://github.com/tfausak))
* [#201](https://github.com/kowainik/relude/issues/201):
  Implement `!!?` as a safe equivalent of `!!` that returns a `Maybe`.
  (by [@kutyel](https://github.com/kutyel))
* [#203](https://github.com/kowainik/relude/issues/203):
  Implement the `guarded` combinator.
  (by [@JonathanLorimer](https://github.com/JonathanLorimer))
* [#214](https://github.com/kowainik/relude/issues/214):
  Add `mapMaybeM` function.
  (by [@vrom911](https://github.com/vrom911))
* [#174](https://github.com/kowainik/relude/issues/174):
  Implement `bimapBoth` in `Relude.Extra.Tuple` module,
  mark `mapBoth` as DEPRECATED.
  (by [@astynax](https://github.com/astynax))
* [#221](https://github.com/kowainik/relude/issues/221):
  Improve documentation for the `Validation` module significantly.
  (by [@chshersh](https://github.com/chshersh))
* [#176](https://github.com/kowainik/relude/issues/176):
  Implement property-based tests for `Validation` laws.
  (by [@astynax](https://github.com/astynax))
* [#172](https://github.com/kowainik/relude/issues/172):
  Add `Monoid` and `Semigroup` instances for the `Validation` type.
  (by [@mauriciofierrom](https://github.com/mauriciofierrom))
* [#156](https://github.com/kowainik/relude/issues/156):
  Implement helper type-level functions in `Relude.Extra.Type`.
  (by [@TheMatten](https://github.com/TheMatten))
* [#170](https://github.com/kowainik/relude/issues/170):
  Implement `Elem` type family.
  (by [@kutyel](https://github.com/kutyel))
* [#165](https://github.com/kowainik/relude/pull/165):
  Re-export `GHC.Float.atan2`.
  (by [@ethercrow](https://github.com/ethercrow))
* [#155](https://github.com/kowainik/relude/issue/155):
  Implement `foldlSC` — short-circuting list fold — in `Relude.Extra.Foldable`.
  (by [@josephcsible](https://github.com/josephcsible))
* [#158](https://github.com/kowainik/relude/issue/158):
  Support GHC-8.6.5.
  (by [@chshersh](https://github.com/chshersh))
* [#148](https://github.com/kowainik/relude/issues/148):
  Migrate HLint rules to the latest Dhall spec.
  (by [@vrom911](https://github.com/vrom911))
* [#178](https://github.com/kowainik/relude/issues/178):
  Made `die` be polymorphic in its return type.
  (by [@ilyakooo0](https://github.com/ilyakooo0))
* [#162](https://github.com/kowainik/relude/pull/162),
  [#189](https://github.com/kowainik/relude/pull/189),
  [#190](https://github.com/kowainik/relude/pull/190),
  [#191](https://github.com/kowainik/relude/pull/191),
  [#193](https://github.com/kowainik/relude/pull/193),
  [#194](https://github.com/kowainik/relude/pull/194),
  [#195](https://github.com/kowainik/relude/pull/195):
  Various refactorings and code improvements:
  + __Breaking change:__ Reorder type parameters to `asumMap`
  + Implement `andM`, `orM`, `allM`, and `anyM` in terms of `&&^` and `||^`
  + Use `foldr` instead of explicit recursion and `toList`
  + Use `mapToFst` instead of `zip` to improve list fusion in `inverseMap`
  + Implement `foldMap1` for `NonEmpty` in terms of `foldr`
  + Use `$>` instead of `*>` and `pure` where possible
  + Implement `asumMap` and `foldMapA` by coercing `foldMap`
  + Return Failure early in `<*` and `*>` too

  (by [@josephcsible](https://github.com/josephcsible))
* [#187](https://github.com/kowainik/relude/issues/187):
  Remove `tasty` and `tasty-hedgehog` dependencies and their redundant imports.
  (by [@dalpd](https://github.com/dalpd))

## 0.5.0 — Mar 18, 2019

* [#127](https://github.com/kowainik/relude/issues/127):
  Implement `Relude.Extra.Lens` module.
* [#125](https://github.com/kowainik/relude/issues/125):
  Moved many numerical functions and types in `Relude.Numeric`.
  Reexport `toIntegralSized` from `Data.Bits`.
  Add `integerToBounded` and `integerToNatural` in `Relude.Numeric`.
* [#121](https://github.com/kowainik/relude/issues/121):
  Reexport `Ap` from `Data.Monoid`. Change definition of `foldMapA` to use `Ap`.
* [#129](https://github.com/kowainik/relude/issues/129):
  Add `appliedTo` and `chainedTo` as named versions of operators `=<<` and `<**>`.
* [#138](https://github.com/kowainik/relude/issues/138):
  Add `RealFloat` to `Relude.Numeric`.
* [#144](https://github.com/kowainik/relude/issues/144):
  Add `traverseToSnd` and friends to `Relude.Extra.Tuple`.
* [#140](https://github.com/kowainik/relude/issues/140):
  Improve text of custom compile-time error messages for `elem` functions.
* [#136](https://github.com/kowainik/relude/issues/136):
  Cover `Relude.Extra.*` modules with custom HLint rules.
* [#146](https://github.com/kowainik/relude/issues/146):
  Improve documentation for `Relude.File` file: be more explicit about system
  locale issues.
* Improve documentation for `One` typeclass and add tests.
* Support ghc-8.6.4 and ghc-8.4.4.
  Drop support for ghc-8.6.1 and ghc-8.4.3.

## 0.4.0 — Nov 6, 2018

* [#70](https://github.com/kowainik/relude/issues/70):
  Reexport `Contravariant` for GHC >= 8.6.1.
* [#103](https://github.com/kowainik/relude/pull/104):
  Drop `utf8-string` dependency and improve performance of conversion functions.
* [#98](https://github.com/kowainik/relude/issues/98):
  Reexport `Bifoldable` related stuff from `base`.
* [#99](https://github.com/kowainik/relude/issues/99):
  Reexport `Bitraversable` related stuff from `base`.
* [#100](https://github.com/kowainik/relude/issues/100):
  Add `Relude.Extra.Validation` with `Validation`data type.
* [#89](https://github.com/kowainik/relude/issues/89):
  Add `Relude.Extra.Type` module containing a `typeName` function.
* [#92](https://github.com/kowainik/relude/issues/92)
  Add `Relude.Extra.Tuple` module, containing
  `dupe`, `mapToFst`, `mapToSnd`, and `mapBoth` functions.
* [#97](https://github.com/kowainik/relude/issues/97):
  Add `(&&^)` and `(||^)` operators.
* [#81](https://github.com/kowainik/relude/issues/81):
  Add `asumMap` to `Foldable` functions.
* [#80](https://github.com/kowainik/relude/issues/80):
  Add hlint rules for `whenLeft`, `whenLeftM`, `whenRight` and `whenRightM`.
* [#79](https://github.com/kowainik/relude/issues/79):
  Add HLint rules for `One` typeclass.
* Remove `openFile` and `hClose`.
* [#83](https://github.com/kowainik/relude/pull/83):
  Make documentation for `nub` functions prettier.
* [#109](https://github.com/kowainik/relude/issues/109):
  Use Dhall v3.0.0 for hlint file generation.

## 0.3.0

* [#41](https://github.com/kowainik/relude/issues/41):
  Add `Foldable1`.
* [#63](https://github.com/kowainik/relude/issues/63):
  Remove `Print` typeclass.
  Add `put[L]BS[Ln]` functions.
  `trace` functions now take `String` as argument instead of `Text`.

  **Important:** this is a breaking change. If you used polymorphic `putStrLn`
  you need to remove type application or switch to one of the monomorphic
  functions. Also, you can't abstract over `Print` typeclass anymore.
* [#66](https://github.com/kowainik/relude/issues/66):
  Export `(>>>)` and `(<<<)` from `Control.Category`.
* [#59](https://github.com/kowainik/relude/issues/59):
  Introduce `flap` function and its operator version `??`.
* [#64](https://github.com/kowainik/relude/issues/64):
  Improve performance of functions from `Foldable1`.
  Add `foldl1'` function.
* Reexport `uncons` from `base`.
* Rewrite `die` implementation to use `die` from `base`.
* [#19](https://github.com/kowainik/relude/issues/19):
  Rewrite `.hlint.yaml` to Dhall.
* Move `stdin`- and `stdout`-related functions to new module `Relude.Lifted.Terminal`.
* [#67](https://github.com/kowainik/relude/issues/67):
  Add HLint rules for `put*` functions.
* [#22](https://github.com/kowainik/relude/issues/22):
  `readFile`, `writeFile` and `appendFile` now work with `String`.
  Add lifted version of `hClose`.
  Add `readFile`, `writeFile` and `appendFile` alternatives for `Text` and `ByteString`.
* [#61](https://github.com/kowainik/relude/issues/61):
  Add `under2` and `underF2` functions to `Relude.Extra.Newtype`.
* [#60](https://github.com/kowainik/relude/issues/60):
  Add `hoistMaybe` and `hoistEither` functions.

## 0.2.0

* [#43](https://github.com/kowainik/relude/issues/43):
  Implement `Relude.Extra.Newtype` module.
* [#46](https://github.com/kowainik/relude/issues/46):
  Add a function that returns its own name.
* [#48](https://github.com/kowainik/relude/issues/48):
  Export `<&>` from `base`.
  Also reexport `fromLeft` and `fromRight` from `base` where possible.
* [#49](https://github.com/kowainik/relude/issues/49):
  Speed up and refactor property tests.
* [#54](https://github.com/kowainik/relude/issues/54):
  Improve documentation.
  Add more examples to documentation and more tests.
  Reexport `withReader` and `withReaderT`.
  Remove `safeHead`.
  Rename `Relude.List.Safe` to `Relude.List.NonEmpty`.

## 0.1.1

* [#44](https://github.com/kowainik/relude/issues/44):
  Implement parser deriviation from pretty-printers.

## 0.1.0

* [#7](https://github.com/kowainik/relude/issues/7):
  Remove `Container.Class.Container`. Export `Foldable`.
* [#2](https://github.com/kowainik/relude/issues/2):
  Remove `microlens` from dependencies.
* [#10](https://github.com/kowainik/relude/issues/10):
  Remove `VarArg` module.
* [#9](https://github.com/kowainik/relude/issues/9):
  Remove `safe-exceptions` from dependencies. Reexport `Exception` and
  `SomeException` from `Control.Exception` instead.
* [#11](https://github.com/kowainik/relude/issues/11):
  Remove `TypeOps` module and `type-operators` dependency.
* [#13](https://github.com/kowainik/relude/issues/13):
  Remove `list`, `getContents`, `interact`, `getArgs`, `note` functions.
  Remove `Lifted.ST` module.
  Rename `Lifted.Env` to `Lifted.Exit`.
* [#16](https://github.com/kowainik/relude/issues/16):
  Rename `whenLeft`, `whenRight`, `whenLeftM`, `whenRightM` to
  `whenLeft_` and `whenRight_`, `whenLeftM_` and `whenRightM_`.
  Add `whenLeft`, `whenRight`, `whenLeftM`, `whenRightM` which return
  the value.
* [#18](https://github.com/kowainik/relude/issues/18):
  Add `LazyStrict` type class for conversions.
* `map` is not `fmap` anymore. Reexport `map` from `Data.List`
* [#12](https://github.com/kowainik/relude/issues/12):
  Remove `liquid-haskell` support.
* [#20](https://github.com/kowainik/relude/issues/20):
  Add `viaNonEmpty` function.
* [#21](https://github.com/kowainik/relude/issues/21):
  Add `MonadFail` instance for `Either`.
* [#17](https://github.com/kowainik/relude/issues/17):
  Add `foldMapA` and `foldMapM` functions.
* [#4](https://github.com/kowainik/relude/issues/4):
  Rename package to `Relude`.
* [#14](https://github.com/kowainik/relude/issues/14):
  Add `Relude.Extra.*` modules which are not exported by default but have useful
  functions.
* [#8](https://github.com/kowainik/relude/issues/8):
  Introduce `StaticMap` and `DynamicMap` type classes as universal interface for
  Map-like structures.

[1]: https://pvp.haskell.org
[2]: https://github.com/kowainik/relude/releases
