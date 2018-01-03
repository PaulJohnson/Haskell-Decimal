Version 0.2.1
-------------

* Fixed `base` dependency.

* Put test suite under `cabal test`

Version 0.2.2
-------------

* Minor fixes to allow compilation under other versions of GHC.

Version 0.2.3
-------------

* Added instance of `NFData` from `Control.DeepSeq`, and hence a dependency
on the `deepseq` package, thanks to Jeff Shaw (shawjef3 at msu.edu).

Version 0.3.1
-------------

* Added `Typeable`, `Fractional` and `RealFrac` instances.

* Multiplication now returns an exact result, increasing precision if necessary.

These changes alter the API. Hence the increment to the major version number.

Thanks to Alexey Uimanov (s9gf4ult at gmail.com).

Version 0.4.1
-------------

* Improved `Read` instance. Now handles `"1.2e3"` and `reads` only returns a single parse.

* Corrected documentation.

* Added `Enum` instance.

* `decimalConvert` now returns a Maybe value. The old version has been renamed
to "unsafeDecimalConvert.

Version 0.5.1
-------------

* Bankers' Rounding implemented in "roundTo". This rounds values ending in "5" to
the nearest even number, in line with the behaviour of "Prelude.round". This
is potentially a breaking change for software that depends on the old
behavior, so the minor version number has been bumped.

* Added a `stack.yaml` file.

* Corrected documentation.

* `Read` instance now handles leading spaces properly.

* Fixed compiler warnings in test suite.

* Added `roundTo'` which allows for `truncate`, `floor` and `ceiling` behaviour when rounding.
