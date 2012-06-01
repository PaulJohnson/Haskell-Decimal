-- | Decimal numbers are represented as @m*10^(-e)@ where
-- @m@ and @e@ are integers.  The exponent @e@ is an unsigned Word8.  Hence
-- the smallest value that can be represented is @10^-255@.
-- 
-- Unary arithmetic results have the exponent of the argument.  Binary
-- arithmetic results have an exponent equal to the maximum of the exponents
-- of the arguments.
-- 
-- Decimal numbers are defined as instances of @Real@.  This means that
-- conventional division is not defined.  Instead the functions @divide@ and 
-- @allocate@ will split a decimal amount into lists of results.  These 
-- results are guaranteed to sum to the original number.  This is a useful
-- property when doing financial arithmetic.
-- 
-- The arithmetic on mantissas is always done using @Integer@, regardless of
-- the type of @DecimalRaw@ being manipulated.  In practice it is recommended
-- that @Decimal@ be used, with other types being used only where necessary
-- (e.g. to conform to a network protocol).

module Data.Decimal (
   -- ** Decimal Values
   DecimalRaw (..),
   Decimal,
   realFracToDecimal,
   decimalConvert,
   roundTo,
   (*.),
   divide,
   allocate,
   -- ** QuickCheck Properties
   prop_readShow,
   prop_readShowPrecision,
   prop_fromIntegerZero,
   prop_increaseDecimals,
   prop_decreaseDecimals,
   prop_inverseAdd,
   prop_repeatedAdd,
   prop_divisionParts,
   prop_divisionUnits,
   prop_allocateParts,
   prop_allocateUnits,
   prop_abs,
   prop_signum
) where

import Control.DeepSeq
import Data.Char
import Data.Ratio
import Data.Word
import Test.QuickCheck
import Text.ParserCombinators.ReadP

-- | Raw decimal arithmetic type constructor.  A decimal value consists of an
-- integer mantissa and a negative exponent which is interpreted as the number
-- of decimal places.  The value stored in a @Decimal d@ is therefore equal to:
-- 
-- > decimalMantissa d / (10 ^ decimalPlaces d)
-- 
-- The "Show" instance will add trailing zeros, so @show $ Decimal 3 1500@
-- will return \"1.500\".  Conversely the "Read" instance will use the decimal
-- places to determine the precision.
-- 
-- Arithmetic and comparision operators convert their arguments to the 
-- greater of the two precisions, and return a result of that precision.  
-- Regardless of the type of the arguments, all mantissa arithmetic is done
-- using @Integer@ types, so application developers do not need to worry about
-- overflow in the internal algorithms.  However the result of each operator
-- will be converted to the mantissa type without checking for overflow.
data (Integral i) => DecimalRaw i = Decimal {
      decimalPlaces :: ! Word8,
      decimalMantissa :: ! i}


-- | Arbitrary precision decimal type.  As a rule programs should do decimal
-- arithmetic with this type and only convert to other instances of 
-- "DecimalRaw" where required by an external interface.
-- 
-- Using this type is also faster because it avoids repeated conversions
-- to and from @Integer@.
type Decimal = DecimalRaw Integer

instance (Integral i, NFData i) => NFData (DecimalRaw i) where
    rnf (Decimal _ i) = rnf i

-- | Convert a real fractional value into a Decimal of the appropriate 
-- precision.
realFracToDecimal :: (Integral i, RealFrac r) => Word8 -> r -> DecimalRaw i
realFracToDecimal e r = Decimal e $ round (r * (10^e))


-- Internal function to divide and return the nearest integer.
divRound :: (Integral a) => a -> a -> a
divRound n1 n2 = if abs r > abs (n2 `quot` 2) then n + signum n else n
    where (n, r) = n1 `quotRem` n2


-- | Convert a @DecimalRaw@ from one base representation to another.  Does
-- not check for overflow in the new representation.
decimalConvert :: (Integral a, Integral b) => DecimalRaw a -> DecimalRaw b
decimalConvert (Decimal e n) = Decimal e $ fromIntegral n


-- | Round a @DecimalRaw@ to a specified number of decimal places.
roundTo :: (Integral i) => Word8 -> DecimalRaw i -> DecimalRaw Integer
roundTo d (Decimal e n) = Decimal d $ fromIntegral n1
    where
      n1 = case compare d e of
             LT -> n `divRound` divisor
             EQ -> n
             GT -> n * multiplier
      divisor = 10 ^ (e-d)
      multiplier = 10 ^ (d-e)


-- Round the two DecimalRaw values to the largest exponent.
roundMax :: (Integral i) => 
            DecimalRaw i -> DecimalRaw i -> (Word8, Integer, Integer)
roundMax d1@(Decimal e1 _) d2@(Decimal e2 _) = (e, n1, n2)
    where
      e = max e1 e2
      (Decimal _ n1) = roundTo e d1
      (Decimal _ n2) = roundTo e d2


instance (Integral i, Show i) => Show (DecimalRaw i) where
   showsPrec _ (Decimal e n)
       | e == 0     = (concat [signStr, strN] ++)
       | otherwise  = (concat [signStr, intPart, ".", fracPart] ++)
       where
         strN = show $ abs n
         signStr = if n < 0 then "-" else ""
         len = length strN
         padded = replicate (fromIntegral e + 1 - len) '0' ++ strN
         (intPart, fracPart) = splitAt (max 1 (len - fromIntegral e)) padded

instance (Integral i, Read i) => Read (DecimalRaw i) where
    readsPrec _ = 
        readP_to_S $ do
          (intPart, _) <- gather $ do
                            optional $ char '-'
                            munch1 isDigit
          fractPart    <- option "" $ do
                            _ <- char '.'
                            munch1 isDigit
          return $ Decimal (fromIntegral $ length fractPart) $ read $ 
                 intPart ++ fractPart


instance (Integral i) => Eq (DecimalRaw i) where
   d1 == d2   =   n1 == n2 where (_, n1, n2) = roundMax d1 d2


instance (Integral i) => Ord (DecimalRaw i) where
    compare d1 d2 = compare n1 n2 where (_, n1, n2) = roundMax d1 d2


instance (Integral i) => Num (DecimalRaw i) where
    d1 + d2 = Decimal e $ fromIntegral (n1 + n2)
        where (e, n1, n2) = roundMax d1 d2
    d1 - d2 = Decimal e $ fromIntegral (n1 - n2)
        where (e, n1, n2) = roundMax d1 d2
    d1 * d2 = Decimal e $ fromIntegral $ 
              (n1 * n2) `divRound` (10 ^ e)
        where (e, n1, n2) = roundMax d1 d2
    abs (Decimal e n) = Decimal e $ abs n
    signum (Decimal _ n) = fromIntegral $ signum n
    fromInteger n = Decimal 0 $ fromIntegral n

instance (Integral i) => Real (DecimalRaw i) where
    toRational (Decimal e n) = fromIntegral n % (10 ^ e)

instance (Integral i, Arbitrary i) => Arbitrary (DecimalRaw i) where
    arbitrary = do
      e <- sized (\n -> resize (n `div` 10) arbitrary) :: Gen Int
      m <- sized (\n -> resize (n * 10) arbitrary)
      return $ Decimal (fromIntegral $ abs e) m
      
instance (Integral i, Arbitrary i) => CoArbitrary (DecimalRaw i) where
    coarbitrary (Decimal e m) gen = variant (v:: Integer) gen
       where v = fromIntegral e + fromIntegral m


-- | Divide a @DecimalRaw@ value into one or more portions.  The portions
-- will be approximately equal, and the sum of the portions is guaranteed to
-- be the original value.
-- 
-- The portions are represented as a list of pairs.  The first part of each
-- pair is the number of portions, and the second part is the portion value.
-- Hence 10 dollars divided 3 ways will produce @[(2, 3.33), (1, 3.34)]@.
divide :: (Integral i) => DecimalRaw i -> Int -> [(Int, DecimalRaw i)]
divide (Decimal e n) d 
    | d > 0 = 
        case n `divMod` fromIntegral d of
          (result, 0) -> [(fromIntegral d, Decimal e result)]
          (result, r) -> [(fromIntegral d - fromIntegral r,
                           Decimal e result), 
                          (fromIntegral r, Decimal e (result+1))]
    | otherwise = error "Data.Decimal.divide: Divisor must be > 0."



-- | Allocate a @DecimalRaw@ value proportionately with the values in a list.
-- The allocated portions are guaranteed to add up to the original value.
-- 
-- Some of the allocations may be zero or negative, but the sum of the list 
-- must not be zero.  The allocation is intended to be as close as possible
-- to the following:
-- 
-- > let result = allocate d parts
-- > in all (== d / sum parts) $ zipWith (/) result parts
allocate :: (Integral i) => DecimalRaw i -> [Integer] -> [DecimalRaw i]
allocate (Decimal e n) ps
    | total == 0  = 
        error "Data.Decimal.allocate: allocation list must not sum to zero."
    | otherwise   = map (Decimal e) $ zipWith (-) ts (tail ts)
    where
      ts = map fst $ scanl nxt (n, total) ps
      nxt (n1, t1) p1 = (n1 - (n1 * fromIntegral p1) `zdiv` t1, 
                         t1 - fromIntegral p1)
      zdiv 0 0 = 0
      zdiv x y = x `divRound` y
      total = fromIntegral $ sum ps


-- | Multiply a @DecimalRaw@ by a @RealFrac@ value.
(*.) :: (Integral i, RealFrac r) => DecimalRaw i -> r -> DecimalRaw i
(Decimal e m) *. d = Decimal e $ round $ fromIntegral m * d


-- | "read" is the inverse of "show".
-- 
-- > read (show n) == n
prop_readShow :: Decimal -> Bool
prop_readShow d =  read (show d) == d

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
