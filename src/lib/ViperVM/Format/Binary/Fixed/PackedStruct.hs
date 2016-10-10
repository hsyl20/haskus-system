{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module ViperVM.Format.Binary.Fixed.PackedStruct
   ( PackedStructLayout
   , StructField
   , PackedStruct
   )
where

import ViperVM.Format.Binary.Fixed.LayoutPtr
import ViperVM.Format.Binary.Fixed.Struct
import ViperVM.Utils.Types
import ViperVM.Format.Binary.Storable

-- | Packed struct with named fields
data PackedStructLayout (fields :: [*])

-- | A packed struct
type PackedStruct (fields :: [*]) = LayoutPtr (PackedStructLayout fields)

type instance LayoutPathType (PackedStructLayout fs) (LayoutPath (LayoutSymbol s ': ps)) = LayoutPathType (StructFieldType fs s) (LayoutPath ps)

type instance LayoutPathOffset (PackedStructLayout fs) (LayoutPath (LayoutSymbol s ': ps)) = PackedStructFieldOffset fs s + LayoutPathOffset (StructFieldType fs s) (LayoutPath ps)


-- | Offset of a record field (with padding taken into account)
type family PackedStructFieldOffset (fs :: [*]) (name :: Symbol) where
   PackedStructFieldOffset fs name = PackedStructFieldOffset' fs name 0

-- | Offset of a record field (with padding taken into account). Accumulated
-- size of the previous fields is passed as last parameter.
type family PackedStructFieldOffset' (fs :: [*]) (name :: Symbol) (sz :: Nat) where
   PackedStructFieldOffset' (StructField name typ ': fs) name sz = sz
   PackedStructFieldOffset' (StructField xx typ ': fs)   name sz =
      PackedStructFieldOffset' fs name (sz + SizeOf typ)
