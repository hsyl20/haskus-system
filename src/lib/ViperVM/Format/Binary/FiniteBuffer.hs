{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Buffer whose size is known at compile time
--
-- TODO: select WordN when the size is small
-- TODO: benchmarks
module ViperVM.Format.Binary.FiniteBuffer
   ( FiniteBuffer
   , withFiniteBufferPtr
   , isFiniteBufferEmpty
   , finiteBufferSize
   )
where


import ViperVM.Format.Binary.Ptr

import GHC.TypeLits
import Data.Proxy

-- | Buffer whose size (in bytes) is known at compile time
newtype FiniteBuffer (n :: Nat) = FiniteBuffer (ForeignPtr ())

-- | Unsafe: be careful if you modify the buffer contents or you may break
-- referential transparency
withFiniteBufferPtr :: FiniteBuffer n -> (Ptr b -> IO a) -> IO a
withFiniteBufferPtr (FiniteBuffer fp) f =
   withForeignPtr fp $ \p -> f (castPtr p)

-- | Test if the buffer is empty
isFiniteBufferEmpty :: forall n. (KnownNat n) => FiniteBuffer n -> Bool
isFiniteBufferEmpty b = finiteBufferSize b == 0

-- | Buffer size
finiteBufferSize :: forall n. (KnownNat n) => FiniteBuffer n -> Word
finiteBufferSize _ = fromIntegral (natVal (Proxy :: Proxy n))
