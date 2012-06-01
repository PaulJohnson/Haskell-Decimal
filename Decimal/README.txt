Variable Precision Decimal Numbers
==================================

The "Decimal" type is mainly intended for doing financial arithmetic
where the number of decimal places may not be known at compile time
(e.g. for a program that handles both Yen and Dollars) and the
application must not drop pennies on the floor.  For instance if you
have to divide $10 between three people then one of them has to get
$3.34.

The number of decimal places in a value is represented as a Word8,
allowing for up to 255 decimal places.  Functions preserve precision.
Binary operators return a result with the precision of the most
precise argument, so 2.3 + 5.678 = 7.978.

If you need fixed precision decimal arithmetic where the precision is
known at compile time then Data.Number.Fixed from Lennart Augustsson's
"numbers" package is more likely to be what you want.

QuickCheck Specification
------------------------

Data.Decimal includes a set of QuickCheck properties which act as both
tests and a formal specification (hence their inclusion in the Haddock
documentation).  To run the tests do:

   cabal configure --enable-tests
   cabal build
   cabal test

Data.Decimal is an instance of Arbitrary, for your convenience in
writing your own tests.


Version 0.2.1
-------------

Fixed "base" dependency.
Put test suite under "cabal test"

Version 0.2.2
-------------

Minor fixes to allow compilation under other versions of GHC.

Version 0.2.3
-------------

Added instance of NFData from Control.DeepSeq, and hence a dependency
on the deepseq package, thanks to Jeff Shaw (shawjef3 at msu.edu).