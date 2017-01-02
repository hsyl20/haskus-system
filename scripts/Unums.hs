{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

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
