{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Haskus.Tests.Format.Binary.Unum
   ( testsUnum
   )
where

import Test.Tasty
import Test.Tasty.QuickCheck

import Haskus.Format.Binary.Unum2
import Haskus.Utils.Flow
import Haskus.Tests.Common

instance Arbitrary Unum3b where
   arbitrary = toUnum <$> arbitrary

testsUnum :: TestTree
testsUnum = testGroup "Unum"
   [ testProperty "bijectivity: negate"
      <| isBijective @Unum3b unumNegate
   , testProperty "bijectivity: reciprocate "
      <| isBijective @Unum3b unumReciprocate
   , testProperty "commutativity: reciprocate/negate"
      <| areCommutative @Unum3b unumReciprocate unumNegate
   , testProperty "reciprocate infinity == zero"
      <| unumReciprocate (unumInfinity :: Unum3b) == unumZero
   , testProperty "reciprocate zero == infinity"
      <| unumReciprocate (unumZero :: Unum3b) == unumInfinity
   ]
