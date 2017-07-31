0.5.1
=====

* Fix an infinite loop in `decodeUtf8` from `Text` to `ByteString.Lazy`.

0.5
=====

* Export `MonadTrans` typeclass.
* Remove `Symbol`-related exports from `GHC.TypeLits`.
* Remove `SrcLoc` and `Location` reexports from `GHC.ExecutionStack`.
* Add `With` type operator.
* Add `hashNub`.
* Export strict `StateT` instead of lazy.

0.4.3
=====

* Assign associativity and priority to (...), export typeclass itself.

0.4.2
=====

* [#25](https://github.com/serokell/universum/issues/25):
  Add vararg functions composition operator (...).
* Rewrite `concatMapM` & `concatForM` so that they allow traversed
  and returned-by-function container types differ.

0.4.1
=====

* Reexport `sortWith` from `GHC.Exts`.

0.4
===

* Add _haddock_ documentation with 100% coverage.
* Rewrite README tutorial.
* [#37](https://github.com/serokell/universum/issues/37):
  Add generalized version of `readEither`.
* [#38](https://github.com/serokell/universum/issues/38):
  Add `evaluateNF`, `evaluateNF_`, `evaluateWHNF`, `evaluateWHNF_`.
* [#39](https://github.com/serokell/universum/issues/39):
  Add lifted versions of `IORef` functions.
* Remove `foreach`
* Reexport `(&&&)` from `Control.Arrow`.
* Add lifted version of `readTVarIO`.
* `interact` and `getContents` work with _Lazy Text_.
* Reexport `MaybeT`, `maybeToExceptT`, `exceptToMaybeT`.

0.3
===

* [#28](https://github.com/serokell/universum/issues/28):
  Remove `putByteString` and `putLByteString`.
* [#29](https://github.com/serokell/universum/issues/29):
  Remove `panic`, `FatalError` and `notImplemented`.
  Rename `NotImplemented` into `Undefined`.
* [#32](https://github.com/serokell/universum/issues/32):
  Remove `orAlt`, `orEmpty`, `liftAA2`, `eitherA`, `purer`, `<<*>>`,
  `traceIO`, `guardM`, `hush`, `tryIO`, `liftM'`, `liftM2'`,
  `applyN`, `guardedA`,
  Bifunctor instances for tuples of length higher than 2.
  Generalize `concatMapM`, add `concatForM` and operator versions.
* [#35](https://github.com/serokell/universum/issues/35):
  Generalize `andM`, `orM`, `allM`, `anyM` over container type.

0.2.2
=====

* [#33](https://github.com/serokell/universum/issues/33):
  Add `($)` and `Each` type operators.

0.2.1
=====

* [#24](https://github.com/serokell/universum/issues/26):
  Add `whenNothing`, `whenNothing_`, `whenNothingM`, `whenNothingM_`,
  `whenLeft`, `whenLeftM`, `whenRight`, `whenRightM`,
  `whenNotNull`, `whenNotNullM`.
* [#26](https://github.com/serokell/universum/issues/24):
   Add `usingReader`, `usingReaderT`,
       `usingState`, `usingStateT`,
       `executingState`, `executingStateT`,
       `evaluatingState`, `evaluatingStateT`.
* Remove `maybeToEither`.

0.2
===

* Add `one` (similar to `singleton`).
* Expose `Symbol` and `Nat` types from `GHC.TypeLits` by default.
* Export `genericLength` and other generic list return functions.
* Rename `msg` to `fatalErrorMessage`.
* Export `ExceptT`
* Export `ReaderT`, and `StateT` constructors.
* Export `NonEmpty` type and constructor for Base 4.9 only.
* Export `Data.Semigroup` type and functions for Base 4.9 only.
* Export `String`.

0.1.13
======

* Add lenses from `microlens`.
* Add `(<&>)`.
* Reexport `(&)` from `Data.Function` if it's present there instead
  of always defining our own (this is actually done by reexporting it
  from `Lens.Micro` which does the right thing).
* Fix a space leak in `whenJust`.

0.1.12
======

* Use custom classes instead of `Foldable`. Thanks to this, `length` and similar functions can't anymore be used on tuples or `Maybe`, but can be used on e.g. `Text`, `ByteString` and `IntSet`.

* Add `allM`, `anyM,` `andM`, `orM`.

* Reexport `fail` and `MonadFail`.

0.1.11
======

* Expose `putByteString` and `putLByteString` monomorphic versions of `putStrLn` functions
* Switch exported `(<>)` to be from `Data.Monoid` instead of Semigroup.
* Export `Hashable`

0.1.10
======

* Generalize most `IO` functions to `MonadIO`
* Make `die` available for older versions of base

0.1.9
=====

* Make `sum` and `product` strict

0.1.8
=====

* ``foreach`` for applicative traversals.
* ``hush`` function for error handling.
* ``tryIO`` function for error handling.
* ``pass`` function for noop applicative branches.
* Mask ``Handler`` typeclass export.
* Mask ``yield`` function export.

0.1.7
=====

* Export monadic ``(>>)`` operator by default.
* Add ``traceId`` and ``traceShowId`` functions.
* Export``reader`` and ``state``  functions by default.
* Export lifted ``throwIO`` and ``throwTo`` functions.

0.1.6
=====

* Add uncatchable panic exception throwing using Text message.
* Remove ``printf``
* Remove ``string-conv`` dependency so Stack build works without ``extra-deps``.
* Bring ``Callstack`` machinery in for GHC 8.x.
* Remove ``throw`` and ``assert`` from ``Control.Exception`` exports.
* Remove ``unsafeShiftL`` and ``unsafeShiftR`` from ``Data.Bits`` exports.
* Reexport ``throw`` as ``unsafeThrow`` via Unsafe module.
* Hides all Show class functions. Only the Class itself is exported. Forbids custom instances that are not GHC derived.
* Export`` encodeUtf8`` and ``decodeUtf8`` functions by default.
* Adds ``unsnoc`` function.

0.1.5
=====

* Initial release.
