{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Memory layout
--
-- Describe a memory region
module ViperVM.Format.Binary.Layout
   ( MemoryLayout (..)
   -- * Layout path
   , LayoutPathType
   , LayoutPathOffset
   , LayoutRoot
   , LayoutPath (..)
   , LayoutIndex (..)
   , LayoutSymbol (..)
   , layoutIndex
   , layoutSymbol
   , (:->)
   , (:#>)
   )
where

import ViperVM.Utils.Types
import ViperVM.Utils.HList
import ViperVM.Format.Binary.Word

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
type instance LayoutPathType l (LayoutPath '[])  = l

-- | Offset obtained when following path p
type family LayoutPathOffset l p :: Nat
type instance LayoutPathOffset e (LayoutPath '[])  = 0

type LayoutRoot = LayoutPath '[]

type family (:->) p (s :: Symbol) where
   (:->) (LayoutPath xs) s = LayoutPath (Snoc xs (LayoutSymbol s))

type family (:#>) p (n :: Nat) where
   (:#>) (LayoutPath xs) n = LayoutPath (Snoc xs (LayoutIndex n))



-- | Data that have a memory layout
class MemoryLayout a where
   -- | Size of the stored data (in bytes)
   type SizeOf a    :: Nat

   -- | Alignment requirement (in bytes)
   type Alignment a :: Nat


instance MemoryLayout Word8 where
   type SizeOf    Word8 = 1
   type Alignment Word8 = 1

instance MemoryLayout Word16 where
   type SizeOf    Word16 = 2
   type Alignment Word16 = 2

instance MemoryLayout Word32 where
   type SizeOf    Word32 = 4
   type Alignment Word32 = 4

instance MemoryLayout Word64 where
   type SizeOf    Word64 = 8
   type Alignment Word64 = 8

instance MemoryLayout Int8 where
   type SizeOf    Int8 = 1
   type Alignment Int8 = 1

instance MemoryLayout Int16 where
   type SizeOf    Int16 = 2
   type Alignment Int16 = 2

instance MemoryLayout Int32 where
   type SizeOf    Int32 = 4
   type Alignment Int32 = 4

instance MemoryLayout Int64 where
   type SizeOf    Int64 = 8
   type Alignment Int64 = 8

