module Haskus.Utils.Maths.Series
   ( factorial
   , sinSeries
   )
where

-- | Factorial
factorial :: Num a => Integer -> a
factorial n
   | n < 0     = error "factorial: n < 0"
   | otherwise = fromInteger $ product [1..n]

-- | MacLaurin series for `sin`
sinSeries :: Fractional a => a -> [a]
sinSeries x = go 0 1
   where
      go n fact = sgn * x^(2*n+1) / fromInteger fact' : go (n+1) fact'
         where
            fact'
               | n == 0    = 1
               | otherwise = fact * (2*n) * (2*n+1)
            sgn   = if even n then 1 else (-1)
