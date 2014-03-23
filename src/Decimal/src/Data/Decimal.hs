{-# LANGUAGE DeriveDataTypeable #-}

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
   eitherFromRational,
   normalizeDecimal
) where


import Control.Monad.Instances ()
import Control.DeepSeq
import Data.Char
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Ratio
import Data.Word
import Data.Typeable
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
                                  deriving (Typeable)


-- | Arbitrary precision decimal type.  As a rule programs should do decimal
-- arithmetic with this type and only convert to other instances of 
-- "DecimalRaw" where required by an external interface.
-- 
-- Using this type is also faster because it avoids repeated conversions
-- to and from @Integer@.
--
-- Note that @reads "12.34e5" :: [Decimal, String]@ will return
-- @[(12, ".234e5"), (12.34, "e5"), (
type Decimal = DecimalRaw Integer

instance (Integral i, NFData i) => NFData (DecimalRaw i) where
    rnf (Decimal _ i) = rnf i

-- | Convert a real fractional value into a Decimal of the appropriate 
-- precision.
realFracToDecimal :: (Integral i, RealFrac r) => Word8 -> r -> DecimalRaw i
realFracToDecimal e r = Decimal e $ round (r * (10^e))


-- Internal function to divide and return the nearest integer. Rounds 0.5 away from zero.
divRound :: (Integral a) => a -> a -> a
divRound n1 n2 = if abs r >= abs (n2 `quot` 2) then n + signum n else n
    where (n, r) = n1 `quotRem` n2


-- | Convert a @DecimalRaw@ from one base representation to another.  Does
-- not check for overflow in the new representation.
decimalConvert :: (Integral a, Integral b) => DecimalRaw a -> DecimalRaw b
decimalConvert (Decimal e n) = Decimal e $ fromIntegral n


-- | Round a @DecimalRaw@ to a specified number of decimal places. 
-- If the value ends in @5@ then it is rounded away from zero. 
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
       | e == 0     = ((signStr ++ strN) ++)
       | otherwise  = (concat [signStr, intPart, ".", fracPart] ++)
       where
         strN = show $ abs n
         signStr = if n < 0 then "-" else ""
         len = length strN
         padded = replicate (fromIntegral e + 1 - len) '0' ++ strN
         (intPart, fracPart) = splitAt (max 1 (len - fromIntegral e)) padded

instance (Integral i, Read i) => Read (DecimalRaw i) where
    readsPrec _ str = readP_to_S readDecimalP str
        

-- | Parse a Decimal value. Used for the Read instance.
readDecimalP :: (Integral i, Read i) => ReadP (DecimalRaw i)
readDecimalP = do
          s1           <- myOpt '+' $ char '-' +++ char '+'
          intPart      <- munch1 isDigit
          fractPart    <- myOpt "" $ do
                            _ <- char '.'
                            munch1 isDigit
          expPart <- myOpt 0 $ do
                            _  <- char 'e' +++ char 'E'
                            s2 <- myOpt '+' $ char '-' +++ char '+'
                            fmap (applySign s2 . strToInt) $ munch1 isDigit
          let n = applySign s1 $ strToInt $ intPart ++ fractPart
              e = length fractPart - expPart
          if e < 0
             then return $ Decimal 0 $ n * 10 ^ negate e
             else if e < 256
                then return $ Decimal (fromIntegral e) n
                else pfail
    where
       strToInt :: (Integral n) => String -> n
       strToInt = foldl (\t v -> 10 * t + v) 0 . map (fromIntegral . subtract (ord '0') . ord)
       applySign '-' v = negate v
       applySign _   v = v
       myOpt d p = p <++ return d


instance (Integral i) => Eq (DecimalRaw i) where
   d1 == d2   =   n1 == n2 where (_, n1, n2) = roundMax d1 d2


instance (Integral i) => Ord (DecimalRaw i) where
    compare d1 d2 = compare n1 n2 where (_, n1, n2) = roundMax d1 d2


instance (Integral i) => Num (DecimalRaw i) where
    d1 + d2 = Decimal e $ fromIntegral (n1 + n2)
        where (e, n1, n2) = roundMax d1 d2
    d1 - d2 = Decimal e $ fromIntegral (n1 - n2)
        where (e, n1, n2) = roundMax d1 d2
    d1 * d2 = normalizeDecimal $ realFracToDecimal maxBound $ toRational d1 * toRational d2

    abs (Decimal e n) = Decimal e $ abs n
    signum (Decimal _ n) = fromIntegral $ signum n
    fromInteger n = Decimal 0 $ fromIntegral n

instance (Integral i) => Real (DecimalRaw i) where
    toRational (Decimal e n) = fromIntegral n % (10 ^ e)

instance (Integral i) => Fractional (DecimalRaw i) where
  fromRational r = normalizeDecimal $ realFracToDecimal maxBound r
  a / b = fromRational $ toRational a / toRational b

instance (Integral i) => RealFrac (DecimalRaw i) where
  properFraction a = (rnd, fromRational rep)
    where
      (rnd, rep) = properFraction $ toRational a
      
  

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

-- | Count the divisors, i.e. the count of 2 divisors in 18 is 1 because 18 = 2 * 3 * 3
factorN :: (Integral a)
           => a                  -- ^ Denominator base
           -> a                  -- ^ dividing value
           -> (a, a)             -- ^ The count of divisors and the result of division
factorN d val = factorN' val 0
  where
    factorN' 1 acc = (acc, 1)
    factorN' v acc = if md == 0
                     then factorN' vd (acc + 1)
                     else (acc, v)
      where
        (vd, md) = v `divMod` d

-- | Try to convert Rational to Decimal with absolute precision
-- return string with fail description if not converted
eitherFromRational :: (Integral i) => Rational -> Either String (DecimalRaw i)
eitherFromRational r = if done == 1
                       then do
                         wres <- we
                         return $ Decimal wres (fromIntegral m)
                       else Left $ show r ++ " has no decimal denominator"
  where
    den = denominator r
    num = numerator r
    (f2, rest) = factorN 2 den
    (f5, done) = factorN 5 rest
    e = max f2 f5
    m = num * ((10^e) `div` den)
    we = if e > fromIntegral (maxBound :: Word8)
         then Left $ show e ++ " is too big ten power to represent as Decimal"
         else Right $ fromIntegral e

-- | Reduce the exponent of the decimal number to the minimal possible value
normalizeDecimal :: (Integral i) => DecimalRaw i -> DecimalRaw i
normalizeDecimal r = case eitherFromRational $ toRational r of
  Right x -> x
  Left e -> error $ "Impossible happened: " ++ e
