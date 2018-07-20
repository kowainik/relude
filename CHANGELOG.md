Change log
==========

0.1.0
=====

* [#8](https://github.com/kowainik/universum/issues/8):
  Introduce `StaticMap` and `DynamicMap` type classes as universal interface for
  Map-like structures.
* [#14](https://github.com/kowainik/universum/issues/14):
  Add `.Extra` modules which are not exported by default but have useful
  functions.
* [#2](https://github.com/kowainik/universum/issues/2):
  Remove `microlens` from dependencies.
* [#12](https://github.com/kowainik/universum/issues/12):
  Remove `liquid-haskell` support.
* [#10](https://github.com/kowainik/universum/issues/10):
  Remove `Universum.VarArg` module.
* [#9](https://github.com/kowainik/universum/issues/9):
  Remove `safe-exceptions` from dependencies. Reexport `Exception` and
  `SomeException` from `Control.Exception` instead.
* [#7](https://github.com/kowainik/universum/issues/7):
  Remove `Universum.Container.Class.Container`. Export `Foldable`.
* [#11](https://github.com/kowainik/universum/issues/11):
  Remove `TypeOps` module and `type-operators` dependency.
* [#13](https://github.com/kowainik/universum/issues/13):
  Remove `list`, `getContents`, `interact`, `getArgs`, `note` functions.
  Remove `Lifted.ST` module.
  Rename `Lifted.Env` to `Lifted.Exit`.
* [#16](https://github.com/kowainik/universum/issues/16):
  Rename `whenLeft`, `whenRight`, `whenLeftM`, `whenRightM` to
  `whenLeft_` and `whenRight_`, `whenLeftM_` and `whenRightM_`.
  Add `whenLeft`, `whenRight`, `whenLeftM`, `whenRightM` which return
  the value.
* [#18](https://github.com/kowainik/universum/issues/18):
  Add `LazyStrict` type class for conversions.
* `map` is not `fmap` anymore. Reexport `map` from `Data.List`
* [#17](https://github.com/kowainik/universum/issues/17):
  Add `foldMapA` and `foldMapM` functions.
* [#20](https://github.com/kowainik/universum/issues/20):
  Add `viaNonEmpty` function.


`universum` uses [PVP Versioning][1].
The change log is available [on GitHub][2].

[1]: https://pvp.haskell.org
[2]: https://github.com/kowainik/universum/releases
