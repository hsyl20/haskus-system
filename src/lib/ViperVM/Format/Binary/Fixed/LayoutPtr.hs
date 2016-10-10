{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Pointer with memory layout
module ViperVM.Format.Binary.Fixed.LayoutPtr
   ( LayoutPtr (..)
   , LayoutPathType
   , LayoutPathOffset
   , castLayoutPtr
   , LayoutRoot
   , LayoutPath (..)
   , LayoutIndex (..)
   , LayoutSymbol (..)
   , layoutIndex
   , layoutSymbol
   , (:->)
   , (:#>)
   , layoutField
   , (-->)
   , (-#>)
   )
where

import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Ptr
import ViperVM.Utils.Types
import ViperVM.Utils.HList

-- | A pointer with a given memory layout
--
-- We use an offset because we can't modify the pointer directly (it is
-- passed to the foreign pointer destructors)
data LayoutPtr l = LayoutPtr {-# UNPACK #-} !(ForeignPtr Word8)
                             {-# UNPACK #-} !Word  -- offset

-- | Path in a layout
data LayoutPath (path :: [*])   = LayoutPath

-- | Index in a layout path
data LayoutIndex (n :: Nat)     = LayoutIndex

-- | Symbol in a layout path
data LayoutSymbol (s :: Symbol) = LayoutSymbol

-- | Index in the layout path
layoutIndex :: forall n. LayoutPath '[LayoutIndex n]
layoutIndex = LayoutPath

-- | Symbol in the layout path
layoutSymbol :: forall s. LayoutPath '[LayoutSymbol s]
layoutSymbol = LayoutPath


-- | Type obtained when following path p
type family LayoutPathType l p :: *

-- | Offset obtained when following path p
type family LayoutPathOffset l p :: Nat
type instance LayoutPathOffset e (LayoutPath '[])  = 0

type LayoutRoot = LayoutPath '[]

type family (:->) p (s :: Symbol) where
   (:->) (LayoutPath xs) s = LayoutPath (Snoc xs (LayoutSymbol s))

type family (:#>) p (n :: Nat) where
   (:#>) (LayoutPath xs) n = LayoutPath (Snoc xs (LayoutIndex n))

-- | Cast a layout pointer
castLayoutPtr :: LayoutPtr l -> LayoutPtr l'
{-# INLINE castLayoutPtr #-}
castLayoutPtr (LayoutPtr fp off) = LayoutPtr fp off

-- | Layout a pointer
layoutField :: forall p l.
   ( KnownNat (LayoutPathOffset l p)
   ) => LayoutPtr l -> p -> LayoutPtr (LayoutPathType l p)
{-# INLINE layoutField #-}
layoutField (LayoutPtr fp off) _ = LayoutPtr fp off'
   where
      off' = off + fromIntegral (natVal (Proxy :: Proxy (LayoutPathOffset l p)))

-- {-# RULES
--  "LayoutPtr concat paths" forall l p1 p2 .
--       layoutField (layoutField l p1) p2 = LayoutField l (concatPaths p1 p2)
--  #-}
-- concatLayoutPaths :: LayoutPath p1 -> LayoutPath p2 -> LayoutPath (Concat p1 p2)
-- concatPaths = undefined


-- | Layout a symbol
(-->) :: forall s l.
   ( KnownNat (LayoutPathOffset l (LayoutPath '[LayoutSymbol s]))
   ) => LayoutPtr l -> LayoutSymbol s -> LayoutPtr (LayoutPathType l (LayoutPath '[LayoutSymbol s]))
{-# INLINE (-->) #-}
(-->) l _ = layoutField l (layoutSymbol :: LayoutPath '[LayoutSymbol s])

-- | Layout an index
(-#>) :: forall n l.
   ( KnownNat (LayoutPathOffset l (LayoutPath '[LayoutIndex n]))
   ) => LayoutPtr l -> LayoutIndex n -> LayoutPtr (LayoutPathType l (LayoutPath '[LayoutIndex n]))
{-# INLINE (-#>) #-}
(-#>) l _ = layoutField l (layoutIndex :: LayoutPath '[LayoutIndex n])
