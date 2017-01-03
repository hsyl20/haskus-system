{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import Haskus.Format.Binary.Unum2

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
