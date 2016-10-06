{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Pointer with memory layout
module ViperVM.Format.Binary.Fixed.LayoutPtr
   ( LayoutPtr (..)
   , Layout (..)
   , Root
   , Path
   , PathIndex
   , PathSymbol
   , (:->)
   , (:#>)
   )
where

import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Ptr
import ViperVM.Utils.Types
import ViperVM.Utils.HList

-- | A pointer with a given memory layout
--
-- We use an offset because we can't modify the pointer directly (it is
-- passed to the destructors)
data LayoutPtr l = LayoutPtr {-# UNPACK #-} !(ForeignPtr Word8)
                             {-# UNPACK #-} !Word  -- offset

data Path (path :: [*])
data PathIndex (n :: Nat)
data PathSymbol (s :: Symbol)

class Layout l where
   -- | Type obtained when following path p
   type FollowPathType l p :: *

   -- | Offset obtained when following path p
   type FollowPathOffset l p :: Nat

type Root = Path '[]

type family (:->) p (s :: Symbol) where
   (:->) (Path xs) s = Path (Snoc xs (PathSymbol s))

type family (:#>) p (n :: Nat) where
   (:#>) (Path xs) n = Path (Snoc xs (PathIndex n))
