{-# LANGUAGE ScopedTypeVariables #-}

module Haskus.Tests.Utils.Maths
   ( testsMaths
   )
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Haskus.Utils.Maths.Series

testsMaths :: TestTree
testsMaths = testGroup "Maths" $
   [ testProperty "Series for sin"
         (testSeries sin sinSeries)
   ]


testSeries :: (Double -> Double) -> (Rational -> [Rational]) -> Rational -> Bool
testSeries op terms x = go 0 (take maxTerms (terms x))
   where
      epsilon  = 1e-5
      maxTerms = 100

      -- witness value (using native sin, cos, etc.)
      wit  = op (fromRational x)

      toV :: Rational -> Double
      toV = fromRational

      go v ts
         | (abs (toV v-wit)) < epsilon = True
         | null ts                     = False
         | otherwise                   = go (v + head ts) (tail ts)
