module Haskus.Tests.Utils where

import Test.Tasty

import Haskus.Tests.Utils.HArray
import Haskus.Tests.Utils.Variant
import Haskus.Tests.Utils.Maths

testsUtils :: TestTree
testsUtils = testGroup "Utils"
   [ testsHArray
   , testsVariant
   , testsMaths
   ]
