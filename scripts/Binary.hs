{-# LANGUAGE DataKinds #-}

import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Fixed.LayoutPtr
import ViperVM.Format.Binary.Fixed.Vector
import Foreign.ForeignPtr
import System.IO.Unsafe

makeData :: Int -> LayoutPtr l
makeData n = unsafePerformIO $ do
   m <- mallocForeignPtrBytes n
   return (LayoutPtr m 0)

d1 :: LayoutPtr (VectorLayout 20 Word8)
d1 = makeData 150

d2 :: LayoutPtr (VectorLayout 20 (VectorLayout 5 Word8))
d2 = makeData 150


p1 :: LayoutPath '[LayoutIndex 10]
p1 = undefined
