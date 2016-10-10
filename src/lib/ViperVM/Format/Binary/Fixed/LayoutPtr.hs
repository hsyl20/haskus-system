{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Pointer with memory layout
module ViperVM.Format.Binary.Fixed.LayoutPtr
   ( LayoutPtr (..)
   , FollowPathType
   , FollowPathOffset
   , castLayoutPtr
   , Root
   , Path
   , PathIndex
   , PathSymbol
   , (:->)
   , (:#>)
   , follow
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

data Path (path :: [*])
data PathIndex (n :: Nat)
data PathSymbol (s :: Symbol)

-- | Type obtained when following path p
type family FollowPathType l p :: *

-- | Offset obtained when following path p
type family FollowPathOffset l p :: Nat
type instance FollowPathOffset e (Path '[])  = 0

type Root = Path '[]

type family (:->) p (s :: Symbol) where
   (:->) (Path xs) s = Path (Snoc xs (PathSymbol s))

type family (:#>) p (n :: Nat) where
   (:#>) (Path xs) n = Path (Snoc xs (PathIndex n))

castLayoutPtr :: LayoutPtr l -> LayoutPtr l'
{-# INLINE castLayoutPtr #-}
castLayoutPtr (LayoutPtr fp off) = LayoutPtr fp off

-- | Follow a pointer
follow :: forall p l.
   ( KnownNat (FollowPathOffset l p)
   ) => LayoutPtr l -> p -> LayoutPtr (FollowPathType l p)
{-# INLINE follow #-}
follow (LayoutPtr fp off) _ = LayoutPtr fp off'
   where
      off' = off + fromIntegral (natVal (Proxy :: Proxy (FollowPathOffset l p)))

-- {-# RULES
--  "LayoutPtr concat paths" forall l p1 p2 .
--       follow (follow l p1) p2 = follow l (concatPaths p1 p2)
--  #-}
-- concatPaths :: Path p1 -> Path p2 -> Path (Concat p1 p2)
-- concatPaths = undefined


-- | Follow a symbol
(-->) :: forall s l.
   ( KnownNat (FollowPathOffset l (Path '[PathSymbol s]))
   ) => LayoutPtr l -> PathSymbol s -> LayoutPtr (FollowPathType l (Path '[PathSymbol s]))
{-# INLINE (-->) #-}
(-->) l _ = follow l (undefined :: Path '[PathSymbol s])

-- | Follow an index
(-#>) :: forall n l.
   ( KnownNat (FollowPathOffset l (Path '[PathIndex n]))
   ) => LayoutPtr l -> PathIndex n -> LayoutPtr (FollowPathType l (Path '[PathIndex n]))
{-# INLINE (-#>) #-}
(-#>) l _ = follow l (undefined :: Path '[PathIndex n])
