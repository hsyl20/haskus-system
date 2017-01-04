{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Haskus.Tests.Format.Binary.Unum
   ( testsUnum
   )
where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Haskus.Format.Binary.Unum2

instance Arbitrary Unum3b where
   arbitrary = toUnum <$> arbitrary

testsUnum :: TestTree
testsUnum = testGroup "Unum" $
   [ testProperty "negate . negate == id" $
         (\(x :: Unum3b) -> unumNegate (unumNegate x) == x)
   , testProperty "reciprocate . reciprocate == id" $
         (\(x :: Unum3b) -> unumReciprocate (unumReciprocate x) == x)
   , testProperty "unumReciprocate unumInfinity == unumZero" $
         (unumReciprocate (unumInfinity :: Unum3b) == unumZero)
   , testProperty "unumReciprocate unumZero == unumInfinity" $
         (unumReciprocate (unumZero :: Unum3b) == unumInfinity)
   ]
