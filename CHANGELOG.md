Change log
==========

0.1.0
=====

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


`universum` uses [PVP Versioning][1].
The change log is available [on GitHub][2].

[1]: https://pvp.haskell.org
[2]: https://github.com/kowainik/universum/releases
