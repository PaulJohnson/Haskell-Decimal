module Main where

import Data.Decimal
import Data.Ratio
import Data.Word
import Test.HUnit

import Test.QuickCheck
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)


-- | Newtype introduced to avoid orphan instance.
newtype TestDecRaw i = Test (DecimalRaw i) deriving Show

type TestDec = TestDecRaw Integer

instance (Integral i, Arbitrary i) => Arbitrary (TestDecRaw i) where
  arbitrary = Test <$> (Decimal <$> arbitrary <*> arbitrary)
  -- arbitrary = do
  --   e <- sized (\n -> resize (n `div` 10) arbitrary) :: Gen Int
  --   m <- sized (\n -> resize (n * 10) arbitrary)
  --   return $ Decimal (fromIntegral $ abs e) m

instance (Integral i, Arbitrary i) => CoArbitrary (TestDecRaw i) where
    coarbitrary (Test (Decimal e m)) = variant (v:: Integer)
       where v = fromIntegral e + fromIntegral m

-- | "read" is the inverse of "show".
--
-- > read (show n) == n
prop_readShow :: TestDec -> Bool
prop_readShow (Test d) =  read (show d) == d


-- | "read" can handle leading spaces.
prop_readShow1 :: TestDec -> Bool
prop_readShow1 (Test d) = read (" " ++ show d) == d

-- | Read and show preserve decimal places.
--
-- > decimalPlaces (read (show n)) == decimalPlaces n
prop_readShowPrecision :: TestDec -> Bool
prop_readShowPrecision (Test d) =  decimalPlaces (read (show d) :: Decimal)
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
prop_increaseDecimals :: TestDec -> Property
prop_increaseDecimals (Test d) =
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
prop_decreaseDecimals :: TestDec -> TestDec -> Bool
prop_decreaseDecimals (Test d1) (Test d2) =  legal beforeRound afterRound
    where
      beforeRound = compare d1 d2
      afterRound = compare (roundTo 0 d1) (roundTo 0 d2)
      legal GT x = x `elem` [GT, EQ]
      legal EQ x = x `elem` [EQ]
      legal LT x = x `elem` [LT, EQ]

-- | @roundTo == roundTo' round@
prop_roundTo :: TestDec -> Word8 -> Bool
prop_roundTo (Test d) e = roundTo' round e d == roundTo e d

-- | > (x + y) - y == x
prop_inverseAdd :: TestDec -> TestDec -> Bool
prop_inverseAdd (Test x) (Test y) =  (x + y) - y == x


-- | Multiplication is repeated addition.
--
-- > forall d, NonNegative i : (sum $ replicate i d) == d * fromIntegral (max i 0)
prop_repeatedAdd :: TestDec -> Word8 -> Bool
prop_repeatedAdd (Test d) i = (sum $ replicate (fromIntegral i) d) == d * fromIntegral (max i 0)


-- | Division produces the right number of parts.
--
-- > forall d, Positive i : (sum $ map fst $ divide d i) == i
prop_divisionParts :: TestDec -> Positive Int -> Property
prop_divisionParts (Test d) (Positive i) =  i > 0 ==> (sum $ map fst $ divide d i) == i


-- | Division doesn't drop any units.
--
-- > forall d, Positive i : (sum $ map (\(n,d1) -> fromIntegral n * d1) $ divide d i) == d
prop_divisionUnits :: TestDec -> Positive Int -> Bool
prop_divisionUnits (Test d) (Positive i) =
    (sum $ map (\(n,d1) -> fromIntegral n * d1) $ divide d i) == d


-- | Allocate produces the right number of parts.
--
-- > sum ps /= 0  ==>  length ps == length (allocate d ps)
prop_allocateParts :: TestDec -> [Integer] -> Property
prop_allocateParts (Test d) ps =
    sum ps /= 0 ==> length ps == length (allocate d ps)


-- | Allocate doesn't drop any units.
--
-- >     sum ps /= 0  ==>  sum (allocate d ps) == d
prop_allocateUnits :: TestDec -> [Integer] -> Property
prop_allocateUnits (Test d) ps =
    sum ps /= 0 ==> sum (allocate d ps) == d

-- | Absolute value definition
--
-- > decimalPlaces a == decimalPlaces d &&
-- > decimalMantissa a == abs (decimalMantissa d)
-- >    where a = abs d
prop_abs :: TestDec -> Bool
prop_abs (Test d) =  decimalPlaces a == decimalPlaces d &&
                     decimalMantissa a == abs (decimalMantissa d)
    where a = abs d

-- | Sign number definition
--
-- > signum d == (fromInteger $ signum $ decimalMantissa d)
prop_signum :: TestDec -> Bool
prop_signum (Test d) =  signum d == (fromInteger $ signum $ decimalMantissa d)

-- | The addition is valid

prop_sumValid :: TestDec -> TestDec -> Property
prop_sumValid (Test a) (Test b) = (decimalPlaces a < maxBound && decimalPlaces b < maxBound) ==>
                    (toRational (a + b) == (toRational a) + (toRational b))

prop_mulValid :: TestDec -> TestDec -> Property
prop_mulValid (Test a) (Test b) = ((ad + bd) < fromIntegral (maxBound :: Word8)) ==>
                    (toRational (a * b) == (toRational a) * (toRational b))
  where
    ad, bd :: Integer
    ad = fromIntegral $ decimalPlaces a
    bd = fromIntegral $ decimalPlaces b

prop_eitherFromRational :: TestDec -> Bool
prop_eitherFromRational (Test d) = (Right d) == (eitherFromRational $ toRational d)

prop_normalizeDecimal :: TestDec -> Bool
prop_normalizeDecimal (Test d) = d == (normalizeDecimal d)


-- | Division is the inverted multiplication
prop_divisionMultiplication :: TestDec -> TestDec -> Property
prop_divisionMultiplication (Test a) (Test b) =
      ((ad + bd) < fromIntegral (maxBound :: Word8) && a /= 0 && b /= 0) ==>
      (c / a == b) .&&. (c / b == a)
  where
    ad :: Integer
    ad = fromIntegral $ decimalPlaces a
    bd = fromIntegral $ decimalPlaces b
    c = a * b

prop_fromRational :: TestDec -> Bool
prop_fromRational (Test a) = a == (fromRational $ toRational a)

prop_properFraction :: TestDec -> Bool
prop_properFraction (Test a) = a == (fromIntegral b + d)
  where
    b :: Integer
    (b, d) = properFraction a

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
                testProperty "readShow1"          prop_readShow1,
                testProperty "readShowPrecision"  prop_readShowPrecision,
                testProperty "fromIntegerZero"    prop_fromIntegerZero,
                testProperty "increaseDecimals"   prop_increaseDecimals,
                testProperty "decreaseDecimals"   prop_decreaseDecimals,
                testProperty "roundTo"            prop_roundTo,
                testProperty "inverseAdd"         prop_inverseAdd,
                testProperty "repeatedAdd"        prop_repeatedAdd,
                testProperty "divisionParts"      prop_divisionParts,
                testProperty "divisionUnits"      prop_divisionUnits,
                testProperty "allocateParts"      prop_allocateParts,
                testProperty "allocateUnits"      prop_allocateUnits,
                testProperty "abs"                prop_abs,
                testProperty "signum"             prop_signum,
                testProperty "sumvalid"           prop_sumValid,
                testProperty "mulValid"           prop_mulValid,
                testProperty "eitherFromRational" prop_eitherFromRational,
                testProperty "normalizeDecimal"   prop_normalizeDecimal,
                testProperty "divisionMultiplication" prop_divisionMultiplication,
                testProperty "fromRational"       prop_fromRational,
                testProperty "properFraction"     prop_properFraction
                ],
        testGroup "Point tests Data.Decimal" [
                testCase "pi to 3dp"     (dec 3 3142  @=? realFracToDecimal 3 piD),
                testCase "pi to 2dp"     (dec 2 314   @=? realFracToDecimal 2 piD),
                testCase "100*pi to 2dp" (dec 2 31416 @=? realFracToDecimal 2 (100 * piD)),
                testCase "1.0 * pi"      (dec 1 31    @=? dec 1 10 *. piD),
                testCase "1.23 * pi"     (dec 2 386   @=? dec 2 123 *. piD),
                testCase "Decimal to DecimalRaw Int"
                                         (decimalConvert (dec 2 123) @=? Just (dec1 2 123)),
                testCase "decimalConvert overflow prevention"
                                         (decimalConvert (1/3 :: Decimal) @=?
                                            (Nothing :: Maybe (DecimalRaw Int))),
                testCase "1.234 to rational" (1234 % 1000 @=? toRational (dec 3 1234)),
                testCase "fromRational (1%10) for DecimalRaw Int"  -- Fixed bug #3
                                         (let v :: DecimalRaw Int
                                              v = fromRational (1%10)
                                          in toRational v @=? 1%10),
                testCase "Bankers rounding up"
                                         (roundTo 1 (dec 2 115) @=? dec 1 12),
                testCase "Bankers rounding down"
                                         (roundTo 1 (dec 2 125) @=? dec 1 12)
                ]
       ]
