{-# LANGUAGE DataKinds #-}

import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Fixed.Buffer
import Foreign.ForeignPtr
import System.IO.Unsafe

makeData :: Int -> Data l
makeData = Data . unsafePerformIO . mallocForeignPtrBytes

d1 :: Data (Vector Word8 20)
d1 = makeData 150

d2 :: Data (Vector (Vector Word8 5) 20)
d2 = makeData 150


p1 :: Path '[PathIndex 10]
p1 = undefined
