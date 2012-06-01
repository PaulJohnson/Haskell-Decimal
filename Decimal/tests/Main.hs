module Main where

import Data.Decimal
import Data.Ratio
import Data.Word
import Test.HUnit



import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)



main :: IO ()
main = defaultMain tests


-- Monomorphic variations on polymorphic themes to avoid type default warnings.

dec :: Word8 -> Integer -> Decimal
dec = Decimal

dec1 :: Word8 -> Int -> DecimalRaw Int
dec1 = Decimal

piD :: Double
piD = pi

tests :: [TF.Test]
tests = [
        testGroup "QuickCheck Data.Decimal" [
                testProperty "readShow"           prop_readShow,
                testProperty "readShowPrecision"  prop_readShowPrecision,
                testProperty "fromIntegerZero"    prop_fromIntegerZero, 
                testProperty "increaseDecimals"   prop_increaseDecimals,
                testProperty "decreaseDecimals"   prop_decreaseDecimals,
                testProperty "inverseAdd"         prop_inverseAdd,
                testProperty "repeatedAdd"        prop_repeatedAdd,
                testProperty "divisionParts"      prop_divisionParts,
                testProperty "divisionUnits"      prop_divisionUnits,
                testProperty "allocateParts"      prop_allocateParts,
                testProperty "allocateUnits"      prop_allocateUnits,
                testProperty "abs"                prop_abs,
                testProperty "signum"             prop_signum
                ],
        testGroup "Point tests Data.Decimal" [
                testCase "pi to 3dp"     (dec 3 3142  @=? realFracToDecimal 3 piD),
                testCase "pi to 2dp"     (dec 2 314   @=? realFracToDecimal 2 piD),
                testCase "100*pi to 2dp" (dec 2 31416 @=? realFracToDecimal 2 (100 * piD)),
                testCase "1.0 * pi"      (dec 1 31    @=? dec 1 10 *. piD),
                testCase "1.23 * pi"     (dec 2 386   @=? dec 2 123 *. piD),
                testCase "Decimal to DecimalRaw Int" 
                                         (decimalConvert (dec 2 123) @=? dec1 2 123),
                testCase "1.234 to rational" (1234 % 1000 @=? (toRational (dec 3 1234)))
                ]
       ]
