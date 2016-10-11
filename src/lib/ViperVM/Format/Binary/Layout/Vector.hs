{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module ViperVM.Format.Binary.Layout.Vector
   ( VectorLayout
   , vectorSlice
   )
where

import ViperVM.Format.Binary.Layout
import ViperVM.Format.Binary.Ptr
import ViperVM.Utils.Types

-- | TODO: (GHC8) replace with TypeError
data OutOfBound

-- | A vector layout of 'n' elements with type 'e'
data VectorLayout (n :: Nat) e

type instance LayoutPathType (VectorLayout n e) (LayoutPath (LayoutIndex i ': ps))  =
   If ((i+1) <=? n)
      (LayoutPathType e (LayoutPath ps))
      OutOfBound

type instance LayoutPathOffset (VectorLayout n e) (LayoutPath (LayoutIndex i ': ps))  =
   -- FIXME: (GHC8) use TypeError with Nat kind
   -- IfNat ((i+1) <=? n)
      (SizeOf e * i + (LayoutPathOffset e (LayoutPath ps)))
   --   OutOfBound

instance MemoryLayout (VectorLayout n e) where
   type SizeOf    (VectorLayout n e) = n * SizeOf e
   type Alignment (VectorLayout n e) = Alignment e


-- | Slice a vector
vectorSlice :: forall n e i m p.
   ( CmpNat (i + m) (n + 1) ~ 'LT -- check that subrange is valid
   , KnownNat (SizeOf e * i)
   , PtrLike p
   ) => p (VectorLayout n e) -> Proxy i -> Proxy m -> p (VectorLayout m e)
{-# INLINE vectorSlice #-}
vectorSlice p _ _ = castPtr (p -#> (LayoutIndex :: LayoutIndex i))
