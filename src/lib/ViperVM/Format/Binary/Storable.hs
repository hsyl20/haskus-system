{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Storable class
module ViperVM.Format.Binary.Storable
   ( FixedStorable (..)
   , RequiredPadding
   , Padding
   , PaddingEx
   , fixedSizeOf
   , fixedAlignment
   )
where

import GHC.TypeLits
import Data.Proxy
import qualified Foreign.Storable as FS

import ViperVM.Format.Binary.Layout
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Ptr
import ViperVM.Utils.Types (Modulo)

-- | Data that can be converted into a Haskell data-type from a pointer
class MemoryLayout a => FixedStorable a where
   -- | Peek (read) a value from a memory address
   fixedPeek :: Ptr a -> IO a

   -- | Poke (write) a value at the given memory address
   fixedPoke :: Ptr a -> a -> IO ()


-- | Compute the required padding between a and b to respect b's alignment
type family RequiredPadding a b where
   RequiredPadding a b = Padding (SizeOf a) b

-- | Compute the required padding between the size sz and b to respect b's alignment
type family Padding (sz :: Nat) b where
   Padding sz b = PaddingEx (Modulo sz (Alignment b)) (Alignment b)

type family PaddingEx (m :: Nat) (a :: Nat) where
   PaddingEx 0 a = 0
   PaddingEx m a = a - m


-- | Get fixedally known size
fixedSizeOf :: forall a v.
   ( FixedStorable a
   , v ~ SizeOf a
   , KnownNat v
   ) => a -> Word
fixedSizeOf _ = fromIntegral (natVal (Proxy :: Proxy v))

-- | Get fixedally known alignment
fixedAlignment :: forall a v.
   ( FixedStorable a
   , v ~ SizeOf a
   , KnownNat v
   ) => a -> Word
fixedAlignment _ = fromIntegral (natVal (Proxy :: Proxy v))


instance FixedStorable Word8 where
   fixedPeek = FS.peek
   fixedPoke = FS.poke

instance FixedStorable Word16 where
   fixedPeek = FS.peek
   fixedPoke = FS.poke

instance FixedStorable Word32 where
   fixedPeek = FS.peek
   fixedPoke = FS.poke

instance FixedStorable Word64 where
   fixedPeek = FS.peek
   fixedPoke = FS.poke

instance FixedStorable Int8 where
   fixedPeek = FS.peek
   fixedPoke = FS.poke

instance FixedStorable Int16 where
   fixedPeek = FS.peek
   fixedPoke = FS.poke

instance FixedStorable Int32 where
   fixedPeek = FS.peek
   fixedPoke = FS.poke

instance FixedStorable Int64 where
   fixedPeek = FS.peek
   fixedPoke = FS.poke
