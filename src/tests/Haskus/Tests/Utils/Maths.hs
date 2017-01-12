{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Haskus.Tests.Utils.Maths
   ( testsMaths
   )
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Haskus.Utils.Maths.Series

testsMaths :: TestTree
testsMaths = testGroup "Maths" $
   [ testProperty "Approximate Pi"
         (converge 100 piShanksSeries pi)
   , testProperty "Approximate exp (tested against host Double)"
         (testSeries 1000 exp expCF)
   , testProperty "Approximate tan (tested against host Double)"
         (testSeries 1000 tan tanCF)
   , testProperty "Approximate sin (tested against host Double)"
         (testSeries 4000 sin sinCF)
   ]

converge :: Word -> [Rational] -> Double -> Bool
converge maxIter rs d 
   | isNaN d      = True
   | isInfinite d = True
   | otherwise    = go maxIter rs
   where
      go :: Word -> [Rational] -> Bool
      go 0 _         = False
      go _ []        = error "Invalid input"
      go n (x:xs)
         | abs ((x+2) / (toRational d + 2)) - 1 < 1e-3 = True
         | otherwise                                   = go (n-1) xs
         

testSeries :: Word -> (Double -> Double) -> (Rational -> [Rational]) -> Rational -> Bool
testSeries maxIter op terms x = converge maxIter (terms x) (op (fromRational x))
