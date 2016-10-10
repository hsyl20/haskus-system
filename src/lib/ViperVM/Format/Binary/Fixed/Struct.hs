{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Structure (named fields with padding for alignment)
module ViperVM.Format.Binary.Fixed.Struct
   ( StructLayout
   , StructField
   , StructFieldType
   , StructFieldOffset
   )
where

import ViperVM.Format.Binary.Fixed.Layout
import ViperVM.Utils.Types
import ViperVM.Format.Binary.Storable

-- | Struct with named fields
data StructLayout (fields :: [*])

-- | Struct field
data StructField (name :: Symbol) (t :: *)

type instance LayoutPathType (StructLayout fs) (LayoutPath (LayoutSymbol s ': ps)) = LayoutPathType (StructFieldType fs s) (LayoutPath ps)

type instance LayoutPathOffset (StructLayout fs) (LayoutPath (LayoutSymbol s ': ps)) = StructFieldOffset fs s + LayoutPathOffset (StructFieldType fs s) (LayoutPath ps)


type family StructFieldType (fs :: [*]) (s :: Symbol) where
   -- TODO: GHC8
   -- StructFieldType '[] name = TypeError "Cannot find field with name " name
   StructFieldType (StructField name t ': fs) name = t
   StructFieldType (StructField name t ': fs) s    = StructFieldType fs s


-- | Offset of a record field (with padding taken into account)
type family StructFieldOffset (fs :: [*]) (name :: Symbol) where
   StructFieldOffset fs name = StructFieldOffset' fs name 0

-- | Offset of a record field (with padding taken into account). Accumulated
-- size of the previous fields + padding is passed as last parameter.
type family StructFieldOffset' (fs :: [*]) (name :: Symbol) (sz :: Nat) where
   StructFieldOffset' (StructField name typ ': fs) name sz = sz + Padding sz typ
   StructFieldOffset' (StructField xx typ ': fs)   name sz =
      StructFieldOffset' fs name (sz + Padding sz typ + SizeOf typ)
