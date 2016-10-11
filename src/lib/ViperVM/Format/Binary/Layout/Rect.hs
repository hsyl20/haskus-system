{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Rectangular region
module ViperVM.Format.Binary.Layout.Rect
   ( RectLayout
   )
where

import ViperVM.Format.Binary.Layout
import ViperVM.Format.Binary.Layout.Vector
import ViperVM.Utils.Types


-- | A rectangle
-- `w`: width in bytes
-- `h`: height in bytes
-- `p`: padding between each row in bytes
data RectLayout (w :: Nat) (h :: Nat) (p :: Nat) e

-- Select the row first
type instance LayoutPathType (RectLayout w h p e) (LayoutPath (LayoutIndex i ': ps))  =
   If ((i+1) <=? h)
      (LayoutPathType (VectorLayout w e) (LayoutPath ps))
      OutOfBound

type instance LayoutPathOffset (RectLayout w h p e) (LayoutPath (LayoutIndex i ': ps))  =
   -- FIXME: (GHC8) use TypeError with Nat kind
   -- IfNat ((i+1) <=? n)
      (SizeOf e * (w + p) * i + (LayoutPathOffset (VectorLayout w e) (LayoutPath ps)))
   --   OutOfBound

instance MemoryLayout (RectLayout w h p e) where
   type SizeOf    (RectLayout w h p e) = h * (w+p) * SizeOf e
   type Alignment (RectLayout w h p e) = Alignment e
