{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Union (overlapping layouts)
module ViperVM.Format.Binary.Layout.Union
   ( UnionLayout
   )
where

import ViperVM.Format.Binary.Layout
import ViperVM.Format.Binary.Layout.NamedField
import ViperVM.Utils.HList

-- | Union of layouts
data UnionLayout (layouts :: [*])

type instance LayoutPathType (UnionLayout fs) (LayoutPath (LayoutSymbol s ': ps)) = LayoutPathType (FieldType fs s) (LayoutPath ps)

type instance LayoutPathOffset (UnionLayout fs) (LayoutPath (LayoutSymbol s ': ps)) = LayoutPathOffset (FieldType fs s) (LayoutPath ps)

instance MemoryLayout (UnionLayout fs) where
   type SizeOf    (UnionLayout fs) = Max (MapSizeOf fs)
   type Alignment (UnionLayout fs) = Max (MapAlignment fs)

type family MapSizeOf fs where
   MapSizeOf '[]                  = '[]
   MapSizeOf (Field name x ': xs) = SizeOf x ': MapSizeOf xs

type family MapAlignment fs where
   MapAlignment '[]                  = '[]
   MapAlignment (Field name x ': xs) = Alignment x ': MapAlignment xs
