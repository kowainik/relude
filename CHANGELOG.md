# Change log

`relude` uses [PVP Versioning][1].
The change log is available [on GitHub][2].

## Unreleased: 0.5.0

* [#125](https://github.com/kowainik/relude/issues/125):
  Moved many numerical functions and types in `Relude.Numeric`.
  Reexport `toIntegralSized` from `Data.Bits`.
  Add `integerToBounded` and `integerToNatural` in `Relude.Numeric`.
* [#129](https://github.com/kowainik/relude/issues/129):
  Add `appliedTo` and `chainedTo` as named versions of operators `=<<` and `<**>`.
* Improve documentation for `One` typeclass and add tests.

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
* [#89](https://github.com/kowainik/relude/issues/81):
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
* [#64](https://github.com/kowainik/relude/issues/64):
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
