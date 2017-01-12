{-# LANGUAGE ScopedTypeVariables #-}

module Haskus.Tests.Utils.Maths
   ( testsMaths
   )
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.Ratio
import Haskus.Utils.Maths.Series

testsMaths :: TestTree
testsMaths = testGroup "Maths" $
   [ testProperty "Series for Pi"
         (converge piShanksSeries pi)
   , testProperty "Series for sin"
         (testSeries sin (euler . sinSeries))
   ]

check :: Rational -> Double -> Bool
check r d = abs (r - toRational d) < epsilon
   where
      epsilon  = 1 % 10000

converge :: [Rational] -> Double -> Bool
converge rs d = go maxIter rs
   where
      maxIter = 100

      go :: Integer -> [Rational] -> Bool
      go 0 _         = False
      go _ []        = error "Invalid input"
      go n (x:xs)
         | check x d = True
         | otherwise = go (n-1) xs
         

testSeries :: (Double -> Double) -> (Rational -> [Rational]) -> Rational -> Bool
testSeries op terms x = converge (terms x) (op (fromRational x))
