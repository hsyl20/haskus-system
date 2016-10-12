{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Structure (named fields with padding for alignment)
module ViperVM.Format.Binary.Layout.Struct
   ( StructLayout
   , Struct
   )
where

import ViperVM.Format.Binary.Layout
import ViperVM.Utils.Types
import ViperVM.Utils.Types.Generics
import ViperVM.Format.Binary.Storable

-- | Struct with named fields
data StructLayout (fields :: [*])

type instance LayoutPathType (StructLayout fs) (LayoutPath (LayoutSymbol s ': ps)) = LayoutPathType (FieldType fs s) (LayoutPath ps)

type instance LayoutPathOffset (StructLayout fs) (LayoutPath (LayoutSymbol s ': ps)) = StructFieldOffset fs s + LayoutPathOffset (FieldType fs s) (LayoutPath ps)


-- | Offset of a record field (with padding taken into account)
type family StructFieldOffset (fs :: [*]) (name :: Symbol) where
   StructFieldOffset fs name = StructFieldOffset' fs name 0

-- | Offset of a record field (with padding taken into account). Accumulated
-- size of the previous fields + padding is passed as last parameter.
type family StructFieldOffset' (fs :: [*]) (name :: Symbol) (sz :: Nat) where
   StructFieldOffset' (Field name typ ': fs) name sz = sz + Padding sz typ
   StructFieldOffset' (Field xx typ ': fs)   name sz =
      StructFieldOffset' fs name (sz + Padding sz typ + SizeOf typ)


instance MemoryLayout (StructLayout fs) where
   type SizeOf    (StructLayout fs) = SizeOfStruct fs
   type Alignment (StructLayout fs) = StructAlignment fs 1

-- | Struct size (with ending padding bytes)
type family SizeOfStruct fs where
   SizeOfStruct fs =
      SizeOfStruct' fs 0
      + PaddingEx (Modulo (SizeOfStruct' fs 0) (StructAlignment fs 1))
         (StructAlignment fs 1)

-- | Get record size without the ending padding bytes
type family SizeOfStruct' (fs :: [*]) (sz :: Nat) where
   SizeOfStruct' '[] sz                    = sz
   SizeOfStruct' (Field name typ ': fs) sz = 
      SizeOfStruct' fs
         (sz
         -- padding bytes
         + Padding sz typ
         -- field size
         + SizeOf typ
         )


-- | Record alignment
type family StructAlignment (fs :: [*]) (a :: Nat) where
   StructAlignment '[]                    a = a
   StructAlignment (Field name typ ': fs) a =
      StructAlignment fs
         (IfNat (a <=? Alignment typ) (Alignment typ) a)

-- | Compute the required padding between a and b to respect b's alignment
type family RequiredPadding a b where
   RequiredPadding a b = Padding (SizeOf a) b

-- | Compute the required padding between the size sz and b to respect b's alignment
type family Padding (sz :: Nat) b where
   Padding sz b = PaddingEx (Modulo sz (Alignment b)) (Alignment b)

type family PaddingEx (m :: Nat) (a :: Nat) where
   PaddingEx 0 a = 0
   PaddingEx m a = a - m



-- | Wrapper for a data-type used as a Struct layout
data Struct a

instance MemoryLayout (Struct a) where
   type SizeOf    (Struct a) = SizeOf    (StructLayout (ExtractFields a))
   type Alignment (Struct a) = Alignment (StructLayout (ExtractFields a))

type instance LayoutPathType (Struct a) (LayoutPath (p ': ps)) = LayoutPathType (StructLayout (ExtractFields a)) (LayoutPath (p ': ps))
type instance LayoutPathOffset (Struct a) (LayoutPath (p ': ps)) = LayoutPathOffset (StructLayout (ExtractFields a)) (LayoutPath (p ': ps))
