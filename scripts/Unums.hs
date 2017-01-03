{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import Haskus.Format.Binary.Unum2
import Haskus.Format.Binary.Bits
import Haskus.Utils.Types

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
      prints :: forall u.
         ( KnownNat (UnumBitCount u)
         , Unum u
         , Num (UnumWord u)
         , Integral (UnumWord u)
         , FiniteBits (UnumWord u)
         , Integral (SORNWord u)
         , FiniteBits (SORNWord u)
         ) => UnumSet u -> IO ()
      prints (AnySet ss) = print (fmap (fromUnum @u) (sornElems ss))

   putStrLn "Test addition"
   prints (unumAdd a1 a2)
   prints (unumAdd a1 a3)
   prints (unumAdd a1 a4)
   prints (unumAdd a5 a5)

   putStrLn "Test multiplication"
   prints (unumMul a1 a2)
   prints (unumMul a2 a2)
   prints (unumMul a3 a3)
   prints (unumMul a1 a1)
   prints (unumMul a1 a3)
   prints (unumMul a1 a6)
   prints (unumMul a1 a4)
   prints (unumMul a5 a5)

   putStrLn "Test division"
   prints (unumDiv a1 a2)
   prints (unumDiv a6 a7)

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
