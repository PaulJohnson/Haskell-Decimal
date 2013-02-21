Haskell-Decimal
===============

Fixed-precision decimal numbers, where the precision is carried with the numbers at run-time.

This is intended for financial arithmetic, so functions are included that allow values to be
split into lists of new values without dropping any pennies on the floor. So $10.00 split 
three ways will produce two lots of $3.33 and one lot of $3.34.
