{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Packed Structure (named fields without padding for alignment)
module ViperVM.Format.Binary.Layout.PackedStruct
   ( PackedStructLayout
   )
where

import ViperVM.Format.Binary.Layout
import ViperVM.Format.Binary.Layout.NamedField
import ViperVM.Utils.Types

-- | Packed struct with named fields
data PackedStructLayout (fields :: [*])

type instance LayoutPathType (PackedStructLayout fs) (LayoutPath (LayoutSymbol s ': ps)) = LayoutPathType (FieldType fs s) (LayoutPath ps)

type instance LayoutPathOffset (PackedStructLayout fs) (LayoutPath (LayoutSymbol s ': ps)) = PackedStructFieldOffset fs s + LayoutPathOffset (FieldType fs s) (LayoutPath ps)


-- | Offset of a record field (with padding taken into account)
type family PackedStructFieldOffset (fs :: [*]) (name :: Symbol) where
   PackedStructFieldOffset fs name = PackedStructFieldOffset' fs name 0

-- | Offset of a record field (with padding taken into account). Accumulated
-- size of the previous fields is passed as last parameter.
type family PackedStructFieldOffset' (fs :: [*]) (name :: Symbol) (sz :: Nat) where
   PackedStructFieldOffset' (Field name typ ': fs) name sz = sz
   PackedStructFieldOffset' (Field xx typ ': fs)   name sz =
      PackedStructFieldOffset' fs name (sz + SizeOf typ)
