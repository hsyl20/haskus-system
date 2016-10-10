{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module ViperVM.Format.Binary.Fixed.Vector
   ( VectorLayout
   , Vector
   , vectorSlice
   )
where

import ViperVM.Format.Binary.Fixed.LayoutPtr
import ViperVM.Utils.Types
import ViperVM.Format.Binary.Storable

-- | TODO: replace with TypeError
data OutOfBound

-- | A vector layout of 'n' elements with type 'e'
data VectorLayout (n :: Nat) e

-- | A vector of 'n' elements with type 'e'
type Vector (n :: Nat) e = LayoutPtr (VectorLayout n e)

type instance LayoutPathType (VectorLayout n e) (LayoutPath (LayoutIndex i ': ps))  =
   If ((i+1) <=? n)
      (LayoutPathType e (LayoutPath ps))
      OutOfBound

type instance LayoutPathOffset (VectorLayout n e) (LayoutPath (LayoutIndex i ': ps))  =
   -- FIXME: (GHC8) use TypeError with Nat kind
   -- IfNat ((i+1) <=? n)
      (SizeOf e * i + (LayoutPathOffset e (LayoutPath ps)))
   --   OutOfBound


-- | Slice a vector
vectorSlice :: forall n e i m.
   ( CmpNat (i + m) (n + 1) ~ 'LT -- check that subrange is valid
   , KnownNat (SizeOf e * i)
   ) => Vector n e -> Proxy i -> Proxy m -> Vector m e
{-# INLINE vectorSlice #-}
vectorSlice v _ _ = castLayoutPtr (layoutField v (LayoutPath :: LayoutPath '[LayoutIndex i]))
