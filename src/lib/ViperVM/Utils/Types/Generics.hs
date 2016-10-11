{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module ViperVM.Utils.Types.Generics
   ( ExtractFields
   , Field
   , FieldType
   )
where

import ViperVM.Utils.Types.List
import GHC.Generics
import ViperVM.Utils.Types

-- | Named field
data Field (name :: Symbol) (t :: *)

type family FieldType (fs :: [*]) (s :: Symbol) where
   -- TODO: GHC8
   -- FieldType '[] name = TypeError "Cannot find field with name " name
   FieldType (Field name t ': fs) name = t
   FieldType (Field name t ': fs) s    = FieldType fs s


-- | Extract fields of a data type as a list of tuples (symbol,type)
type family ExtractFields (a :: *)  where
   ExtractFields a = ExtractFields' (Rep a)

type family ExtractFields' a where
   -- extract constructors
   ExtractFields' (D1 _ cs) = ExtractFields' cs

   -- extract selectors (only support a single constructor for now)
   ExtractFields' (C1 _ ss) = ExtractFields' ss

   -- support several selectors
   ExtractFields' (s1 :*: s2) = Concat (ExtractFields' s1) (ExtractFields' s2)

   -- extract field name and type from the selector
   --    - there must be a name
   --    - there mustn't be recursive types
   ExtractFields' (S1 ('MetaSel ('Just name) _ _ _) (Rec0 t)) = '[Field name t]
