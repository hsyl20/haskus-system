-- | Helper class to pass parameters to system calls
module ViperVM.Arch.Linux.Internals.Arg
   ( Arg (..)
   )
where

import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Ptr (Ptr, ptrToIntPtr)


-- | Parameters that can be directly passed to system calls
class Arg a where
   toArg :: a -> Int64

instance Arg Int     where toArg = fromIntegral
instance Arg Int64   where toArg = id
instance Arg Word    where toArg = fromIntegral
instance Arg Word64  where toArg = fromIntegral
instance Arg Word32  where toArg = fromIntegral
instance Arg CUShort where toArg = fromIntegral
instance Arg (Ptr a) where toArg = fromIntegral . ptrToIntPtr
