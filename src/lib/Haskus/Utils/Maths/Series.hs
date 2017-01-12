{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Haskus.Utils.Maths.Series
   ( partialSums
   , factorial
   , euler
   , shanks
   , shanks3
   , shanksN
   , piSeries
   , piShanksSeries
   , sinSeries
   , sinShanksSeries
   , cosSeries
   , cosShanksSeries
   , continuedFraction
   , generalizedContinuedFraction
   , sineCF
   , sineBounds
   , sinReduce
   , cosReduce
   )
where

import Prelude hiding (pi)

-- | Factorial
factorial :: Num a => Integer -> a
factorial x = fromInteger $ product [1..x]

-- | Partial sums
partialSums :: Num a => [a] -> [a]
partialSums = scanl1 (+)

-- | Shanks transformation for series acceleration
shanks :: Fractional a => [a] -> [a]
shanks xs@(x:y:z:_) = (x * z - y * y) / (z - 2 * y + x) : shanks (tail xs)
shanks _            = error "shanks: invalid input"

-- | Shanks transformation applied 3-times
shanks3 :: Fractional a => [a] -> [a]
shanks3 = shanks . shanks . shanks

-- | Shanks transformation applied 3-times
shanksN :: Fractional a => Word -> [a] -> [a]
shanksN 0 xs = xs
shanksN n xs = shanksN (n-1) (shanks xs)

-- | Euler's transform
euler :: (Fractional a, Num a) => [a] -> [a]
euler ys@(x:xs) = x : zipWith (\u v -> (u+v)/2) ys xs
euler [] = error "Invalid input"


-- | Leibniz formula for Pi
piSeries :: Fractional a => [a]
piSeries = partialSums (go 0)
   where
      go !n = 4 * sgn / fromInteger (2*n+1) : go (n+1)
         where
            sgn   = if even n then 1 else (-1)

-- | Leibniz formula for Pi through Shanks transformation (3-times)
piShanksSeries :: Fractional a => [a]
piShanksSeries = shanks3 piSeries


-- | MacLaurin series for `sin`
sinSeries :: (Eq a, Fractional a) => a -> [a]
sinSeries x
   | x == 0    = 0 : repeat 0
   | otherwise = go 0 1
   where
      go n fact = sgn * x^(2*n+1) / fromInteger fact' : go (n+1) fact'
         where
            fact'
               | n == 0    = 1
               | otherwise = fact * (2*n) * (2*n+1)
            sgn   = if even n then 1 else (-1)


-- | MacLaurin series for `sin` through Shanks transformation (3-times)
sinShanksSeries :: (Eq a, Fractional a) => a -> [a]
sinShanksSeries x
   | x == 0    = 0 : repeat 0
   | otherwise = shanksN 3 (sinSeries x)


-- | MacLaurin series for `cos`
cosSeries :: (Eq a, Fractional a) => a -> [a]
cosSeries x
   | x == 0    = 1 : repeat 0
   | otherwise = go 0 1
   where
      go n fact = sgn * x^(2*n) / fromInteger fact' : go (n+1) fact'
         where
            fact'
               | n == 0    = 1
               | otherwise = fact * (2*n) * (2*n-1)
            sgn   = if even n then 1 else (-1)


-- | MacLaurin series for `cos` through Shanks transformation (3-times)
cosShanksSeries :: (Eq a, Fractional a) => a -> [a]
cosShanksSeries x
   | x == 0    = 1 : repeat 0
   | otherwise = shanksN 3 (cosSeries x)

-- Sin and Cos
-- ~~~~~~~~~~~
-- 
-- For all x, we can find x = 2^n * x' so that |x'| <= 1
--
-- Then we can use Taylor series to find sin(x') and cos(x').
--
-- We have sin(2x) = 2 * sin x * cos x
--     and cos(2x) = (cos x)^2 - (sin x)^2
--
-- So we can use these expressions to compute sin(x) and cos(x) from sin(x') and
-- cos(x').
--
-- Note that (cos x)^2 + (sin x)^2 = 1

sinReduce :: Num a => Rational -> (Rational -> a) -> (Rational -> a) -> Rational -> a
sinReduce n sin' cos' x
   | x > n     = 2 * sinReduce n sin' cos' (x / 2) * cosReduce n sin' cos' (x / 2)
   | otherwise = sin' x

cosReduce :: Num a => Rational -> (Rational -> a) -> (Rational -> a) -> Rational -> a
cosReduce n sin' cos' x
   | x > n     = let c = cosReduce n sin' cos' (x / 2)
                     s = sinReduce n sin' cos' (x / 2)
                 in c*c - s*s
   | otherwise = cos' x


-- Continued fractions
-- ~~~~~~~~~~~~~~~~~~~
--

-- | Compute a list of convergents Ci of a continued fraction from a list of partial quotients
continuedFraction :: [Rational] -> [Rational]
continuedFraction an = fmap (\(pi,qi) -> pi / qi) (tail pq)
   where
      -- (pi,qi) for all i >= -1
      pq = (1,0) : (head an,1) : f pq (tail an)
      
      f pq'@(~((pi2,qi2):(pi1,qi1):_)) ~(ai:as) = (pi,qi) : f (tail pq') as
         where
            pi = ai * pi1 + pi2
            qi = ai * qi1 + qi2

-- | Compute a list of convergents Ci of a generalized continued fraction from a
-- list of partial numerators and partial denominators
--
-- 'an' for n >= 0 is a list of partial denominators (as in the non-general case)
-- 'bn' for n >= 1 is a list of partial numerators (bn = repeat 1 in the
-- non-general case)
generalizedContinuedFraction :: [Rational] -> [Rational] -> [Rational]
generalizedContinuedFraction an bn = fmap (\(pi,qi) -> pi / qi) (tail pq)
   where
      -- (pi,qi) for all i >= -1
      pq = (1,0) : (head an,1) : f pq (tail an) bn
      
      f pq'@(~((pi2,qi2):(pi1,qi1):_)) ~(ai:as) ~(bi:bs) = (pi,qi) : f (tail pq') as bs
         where
            pi = ai * pi1 + bi * pi2
            qi = ai * qi1 + bi * qi2

-- | Convergents of Sine's continued fraction
sineCF :: Rational -> [Rational]
sineCF x = generalizedContinuedFraction an bn
   where
      an = 0 : 1   : [ n*(n+1) - x*x | n <- [2,4..] ]
      bn = x : x*x : [ n*(n+1) * x*x | n <- [2,4..] ]


-- | Approximate sin(x) with Sine's continued fraction
sineBounds :: Rational -> [(Rational,Rational)]
sineBounds = go . sineCF
   where
      go ~(x:y:zs) = (x,y):go zs
