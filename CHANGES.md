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
