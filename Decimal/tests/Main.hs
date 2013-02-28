module Main where

import Data.Decimal
import Data.Ratio
import Data.Word
import Test.HUnit
import Control.Applicative

import Test.QuickCheck
import qualified Test.QuickCheck.Property as P
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)


instance (Integral i, Arbitrary i) => Arbitrary (DecimalRaw i) where
  arbitrary = Decimal <$> arbitrary <*> arbitrary
  -- arbitrary = do 
  --   e <- sized (\n -> resize (n `div` 10) arbitrary) :: Gen Int
  --   m <- sized (\n -> resize (n * 10) arbitrary)
  --   return $ Decimal (fromIntegral $ abs e) m
      
instance (Integral i, Arbitrary i) => CoArbitrary (DecimalRaw i) where
    coarbitrary (Decimal e m) gen = variant (v:: Integer) gen
       where v = fromIntegral e + fromIntegral m
  
-- | "read" is the inverse of "show".
-- 
-- > read (show n) == n
prop_readShow :: Decimal -> Bool
prop_readShow d =  (read (show d)) == d

-- | Read and show preserve decimal places.
-- 
-- > decimalPlaces (read (show n)) == decimalPlaces n
prop_readShowPrecision :: Decimal -> Bool
prop_readShowPrecision d =  decimalPlaces (read (show d) :: Decimal) 
                            == decimalPlaces d


-- | "fromInteger" definition.
-- 
-- > decimalPlaces (fromInteger n) == 0 &&
-- > decimalMantissa (fromInteger n) == n
prop_fromIntegerZero :: Integer -> Bool
prop_fromIntegerZero n =  decimalPlaces (fromInteger n :: Decimal) == 0 &&
                          decimalMantissa (fromInteger n :: Decimal) == n


-- | Increased precision does not affect equality.
-- 
-- > decimalPlaces d < maxBound ==> roundTo (decimalPlaces d + 1) d == d
prop_increaseDecimals :: Decimal -> Property
prop_increaseDecimals d =  
    decimalPlaces d < maxBound ==> roundTo (decimalPlaces d + 1) d == d


-- | Decreased precision can make two decimals equal, but it can never change
-- their order.
-- 
-- > forAll d1, d2 :: Decimal -> legal beforeRound afterRound
-- >      where
-- >         beforeRound = compare d1 d2
-- >         afterRound = compare (roundTo 0 d1) (roundTo 0 d2)
-- >         legal GT x = x `elem` [GT, EQ]
-- >         legal EQ x = x `elem` [EQ]
-- >         legal LT x = x `elem` [LT, EQ]
prop_decreaseDecimals :: Decimal -> Decimal -> Bool
prop_decreaseDecimals d1 d2 =  legal beforeRound afterRound
    where
      beforeRound = compare d1 d2
      afterRound = compare (roundTo 0 d1) (roundTo 0 d2)
      legal GT x = x `elem` [GT, EQ]
      legal EQ x = x `elem` [EQ]
      legal LT x = x `elem` [LT, EQ]


-- | > (x + y) - y == x
prop_inverseAdd :: Decimal -> Decimal -> Bool
prop_inverseAdd x y =  (x + y) - y == x


-- | Multiplication is repeated addition.
-- 
-- > forall d, NonNegative i : (sum $ replicate i d) == d * fromIntegral (max i 0)
prop_repeatedAdd :: Decimal -> Word8 -> Bool
prop_repeatedAdd d i = (sum $ replicate (fromIntegral i) d) == d * fromIntegral (max i 0)


-- | Division produces the right number of parts.
-- 
-- > forall d, Positive i : (sum $ map fst $ divide d i) == i
prop_divisionParts :: Decimal -> Positive Int -> Property
prop_divisionParts d (Positive i) =  i > 0 ==> (sum $ map fst $ divide d i) == i


-- | Division doesn't drop any units.
-- 
-- > forall d, Positive i : (sum $ map (\(n,d1) -> fromIntegral n * d1) $ divide d i) == d
prop_divisionUnits :: Decimal -> Positive Int -> Bool
prop_divisionUnits d (Positive i) = 
    (sum $ map (\(n,d1) -> fromIntegral n * d1) $ divide d i) == d


-- | Allocate produces the right number of parts.
-- 
-- > sum ps /= 0  ==>  length ps == length (allocate d ps)
prop_allocateParts :: Decimal -> [Integer] -> Property
prop_allocateParts d ps =  
    sum ps /= 0 ==> length ps == length (allocate d ps)


-- | Allocate doesn't drop any units.
-- 
-- >     sum ps /= 0  ==>  sum (allocate d ps) == d
prop_allocateUnits :: Decimal -> [Integer] -> Property
prop_allocateUnits d ps =
    sum ps /= 0 ==> sum (allocate d ps) == d

-- | Absolute value definition
-- 
-- > decimalPlaces a == decimalPlaces d && 
-- > decimalMantissa a == abs (decimalMantissa d)
-- >    where a = abs d
prop_abs :: Decimal -> Bool
prop_abs d =  decimalPlaces a == decimalPlaces d && 
              decimalMantissa a == abs (decimalMantissa d)
    where a = abs d

-- | Sign number defintion
-- 
-- > signum d == (fromInteger $ signum $ decimalMantissa d)
prop_signum :: Decimal -> Bool
prop_signum d =  signum d == (fromInteger $ signum $ decimalMantissa d)

-- | The addition is valid
                 
prop_sumValid :: Decimal -> Decimal -> Property
prop_sumValid a b = (decimalPlaces a < maxBound && decimalPlaces b < maxBound) ==>
                    (toRational (a + b) == (toRational a) + (toRational b))

prop_mulValid :: Decimal -> Decimal -> Property
prop_mulValid a b = (decimalPlaces a + decimalPlaces b < maxBound) ==>
                    (toRational (a * b) == (toRational a) * (toRational b))


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
                testProperty "signum"             prop_signum,
                testProperty "sumvalid"           prop_sumValid,
                testProperty "mulValid"           prop_mulValid
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
