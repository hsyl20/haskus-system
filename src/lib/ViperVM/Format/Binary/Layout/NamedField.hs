{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Named fields (used for structs, unions, etc.)
module ViperVM.Format.Binary.Layout.NamedField
   ( Field
   , FieldType
   )
where

import ViperVM.Utils.Types

-- | Named field
data Field (name :: Symbol) (t :: *)

type family FieldType (fs :: [*]) (s :: Symbol) where
   -- TODO: GHC8
   -- FieldType '[] name = TypeError "Cannot find field with name " name
   FieldType (Field name t ': fs) name = t
   FieldType (Field name t ': fs) s    = FieldType fs s
