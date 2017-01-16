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

* Add specialized print functions for `ByteString`
* Export more stuff from `Semigroup` and use `(<>)` from `Monoid`
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

* Exports monadic ``(>>)`` operator by default.
* Adds ``traceId`` and ``traceShowId`` functions.
* Exports``reader`` and ``state``  functions by default.
* Export lifted ``throwIO`` and ``throwTo`` functions.

0.1.6
=====

* Adds uncatchable panic exception throwing using Text message.
* Removes ``printf``
* Removes ``string-conv`` dependency so Stack build works without ``extra-deps``.
* Brings ``Callstack`` machinery in for GHC 8.x.
* Removes ``throw`` and ``assert`` from ``Control.Exception`` exports.
* Removes ``unsafeShiftL`` and ``unsafeShiftR`` from ``Data.Bits`` exports.
* Reexport ``throw`` as ``unsafeThrow`` via Unsafe module.
* Hides all Show class functions. Only the Class itself is exported. Forbids custom instances that are not GHC derived.
* Export`` encodeUtf8`` and ``decodeUtf8`` functions by default.
* Adds ``unsnoc`` function.

0.1.5
=====
