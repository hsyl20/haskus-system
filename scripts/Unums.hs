{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import Haskus.Format.Binary.Unum2
import GHC.Real

main :: IO ()
main = do
   print (unumExactMembers (undefined :: Unum2b))
   print (toUnum 0 :: Unum2b)
   print (toUnum 10 :: Unum2b)
   print (toUnum (-5) :: Unum2b)
   print (unumNegate (toUnum (-5) :: Unum2b))
   print (unumReciprocate (toUnum (-5) :: Unum2b))

   print (unumExactMembers (undefined :: Unum3b))
   print (toUnum 0 :: Unum3b)
   print (toUnum 0.5 :: Unum3b)
   print (toUnum 1 :: Unum3b)
   print (toUnum 10 :: Unum3b)
   print (toUnum (-0.8) :: Unum3b)
   print (toUnum (-1) :: Unum3b)
   print (toUnum (-1.1) :: Unum3b)
   print (unumReciprocate (toUnum 0) :: Unum3b)
   print (unumNegate (toUnum (-1) :: Unum3b))
   print (unumReciprocate (toUnum (-1) :: Unum3b))

   print (fromUnum (toUnum (-1) :: Unum3b))
   print (fromUnum (toUnum (-1.5) :: Unum3b))

   let
      a1 = toUnum (-1)
      a2 = toUnum (0)
      a3 = toUnum (1)
      a4 = toUnum (1.5)
      a5 = toUnum (-3.5)
      a6 = toUnum (0.5)
      a7 = toUnum (0.6)
      s = sornFromList [a1,a2,a3,a4,a5,a6,a7] :: SORN Unum3b
   print s
   print (fmap fromUnum (sornElems s))

   let
      exs = sornFromList (filter unumIsExactNumber (sornElems (sornFull @Unum3b)))
   print exs


   putStrLn "Test addition"
   print (unumAdd a1 a2)
   print (unumAdd a1 a3)
   print (unumAdd a1 a4)
   print (unumAdd a5 a5)

   putStrLn "Test multiplication"
   print (unumMul a1 a2)
   print (unumMul a2 a2)
   print (unumMul a3 a3)
   print (unumMul a1 a1)
   print (unumMul a1 a3)
   print (unumMul a1 a6)
   print (unumMul a1 a4)
   print (unumMul a5 a5)

   putStrLn "Test division"
   print (unumDiv a1 a2)
   print (unumDiv a6 a7)

   putStrLn "Test lifted operations"

   let
      addDep   = unumLiftOpDep @Unum3b unumAdd
      addIndep = unumLiftOpIndep @Unum3b unumAdd
      subDep   = unumLiftOpDep @Unum3b unumSub
      subIndep = unumLiftOpIndep @Unum3b unumSub
      mulDep   = unumLiftOpDep @Unum3b unumMul
      mulIndep = unumLiftOpIndep @Unum3b unumMul
      divDep   = unumLiftOpDep @Unum3b unumDiv
      divIndep = unumLiftOpIndep @Unum3b unumDiv

   print (addIndep (unumMul a1 a6) (unumDiv a5 a7))
   print (addIndep (unumMul a1 a6) (unumMul a5 a7))


   putStrLn "Kinematics test"
   let
      constU :: Rational -> UnumSet Unum3b
      constU x = constV (toUnum x)

      constV :: Unum3b  -> UnumSet Unum3b
      constV x = AnySet (sornInsert (sornEmpty @Unum3b) x)

      testKin :: [UnumSet Unum3b] -> [UnumSet Unum3b] -> Bool
      testKin cs@[c1,c2,c3,c4,c5,c6] ss@[s1,s2,s3,s4,s5,s6] =
         ( (s2 + s3 + s4 + s2 + s3 + s2                    `unumSetMember` toUnum (39701 % 10000))
         && (all (\(ci,si) -> mulDep si + mulDep ci  `unumSetMember` toUnum 1) (cs `zip` ss))
         && (s2*c5*s6 - s3*c5*s6 - s4*c5*c6 + c2*c6 +c3*c6 +c4*c6 `unumSetMember` toUnum (4077 % 10000))
         && (c1*c2*s5 + c1*c3*s5 + c1*c4*s5 + s1*c5        `unumSetMember` toUnum (19115 % 10000))
         && (s2*s5 + s3*s5 + s4*s5                         `unumSetMember` toUnum (19791 % 10000))
         && (c1*c2 + c1*c3 + c1*c4 + c1*c2 + c1*c3 + c1*c2 `unumSetMember` toUnum (40616 % 10000))
         && (s1*c2 + s1*c3 + s1*c4 + s1*c2 + s1*c3 + s1*c2 `unumSetMember` toUnum (17172 % 10000))
         )

      vs = fmap constV (unumRange (toUnum (-1)) (toUnum 1))

      vs' = [ ([c1,c2,c3,c4,c5,c6],[s1,s2,s3,s4,s5,s6])
            | c1 <- vs
            , c2 <- vs
            , c3 <- vs
            , c4 <- vs
            , c5 <- vs
            , c6 <- vs
            , s1 <- vs
            , s2 <- vs
            , s3 <- vs
            , s4 <- vs
            , s5 <- vs
            , s6 <- vs
            ]

   print (length (filter (uncurry testKin) vs'))

instance Num (UnumSet Unum3b) where
   (*) = unumLiftOpIndep @Unum3b unumMul
   (+) = unumLiftOpIndep @Unum3b unumAdd
   (-) = unumLiftOpIndep @Unum3b unumSub

instance Fractional (UnumSet Unum3b) where
   (/) = unumLiftOpIndep @Unum3b unumDiv
