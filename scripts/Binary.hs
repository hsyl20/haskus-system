{-# LANGUAGE DataKinds #-}

import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Ptr
import ViperVM.Format.Binary.Layout.Vector
import ViperVM.Format.Binary.Layout
import System.IO.Unsafe

makeData :: Int -> FinalizedPtr l
makeData n = unsafePerformIO $ do
   m <- mallocForeignPtrBytes n
   return (FinalizedPtr m 0)

d0 :: Ptr (VectorLayout 20 Word8)
d0 = nullPtr

d1 :: FinalizedPtr (VectorLayout 20 Word8)
d1 = makeData 150

d2 :: FinalizedPtr (VectorLayout 20 (VectorLayout 5 Word32))
d2 = makeData 150


p1 :: LayoutPath '[LayoutIndex 10]
p1 = undefined
