{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module ViperVM.Format.Binary.Fixed.Vector
   ( VectorLayout
   , Vector
   , slice
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

type instance FollowPathType (VectorLayout n e) (Path (PathIndex i ': ps))  =
   If ((i+1) <=? n)
      (FollowPathType e (Path ps))
      OutOfBound

type instance FollowPathOffset (VectorLayout n e) (Path (PathIndex i ': ps))  =
   -- FIXME: use TypeError with Nat kind
   -- IfNat ((i+1) <=? n)
      (SizeOf e * i + (FollowPathOffset e (Path ps)))
   --   OutOfBound


-- | Slice a vector
slice :: forall n e i m.
   ( CmpNat (i + m) (n + 1) ~ 'LT -- check that subrange is valid
   , KnownNat (SizeOf e * i)
   ) => Vector n e -> Proxy i -> Proxy m -> Vector m e
slice v _ _ = castLayoutPtr (follow v (undefined :: Path '[PathIndex i]))
