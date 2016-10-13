{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Structure (named fields with padding for alignment)
module ViperVM.Format.Binary.Layout.Struct
   ( StructLayout
   , Struct (..)
   , withStruct
   , allocaStruct
   , peekStruct
   -- , peekFields
   -- , peekStruct
   -- , peekStructCons
   )
where

import ViperVM.Format.Binary.Layout
import ViperVM.Format.Binary.Storable
import ViperVM.Utils.Types
import ViperVM.Utils.Types.Generics
import ViperVM.Format.Binary.Ptr

-- | Struct with named fields
data StructLayout (fields :: [*])

type instance LayoutPathType (StructLayout fs) (LayoutPath (LayoutSymbol s ': ps)) = LayoutPathType (FieldType fs s) (LayoutPath ps)

type instance LayoutPathOffset (StructLayout fs) (LayoutPath (LayoutSymbol s ': ps)) = StructFieldOffset fs s + LayoutPathOffset (FieldType fs s) (LayoutPath ps)


-- | Offset of a record field (with padding taken into account)
type family StructFieldOffset (fs :: [*]) (name :: Symbol) where
   StructFieldOffset fs name = StructFieldOffset' fs name 0

-- | Offset of a record field (with padding taken into account). Accumulated
-- size of the previous fields + padding is passed as last parameter.
type family StructFieldOffset' (fs :: [*]) (name :: Symbol) (sz :: Nat) where
   StructFieldOffset' (Field name typ ': fs) name sz = sz + Padding sz typ
   StructFieldOffset' (Field xx typ ': fs)   name sz =
      StructFieldOffset' fs name (sz + Padding sz typ + SizeOf typ)


-- | Struct size (with ending padding bytes)
type family SizeOfStruct fs where
   SizeOfStruct fs =
      SizeOfStruct' fs 0
      + PaddingEx (Modulo (SizeOfStruct' fs 0) (StructAlignment fs))
         (StructAlignment fs)

-- | Get record size without the ending padding bytes
type family SizeOfStruct' (fs :: [*]) (sz :: Nat) where
   SizeOfStruct' '[] sz                    = sz
   SizeOfStruct' (Field name typ ': fs) sz = 
      SizeOfStruct' fs
         (sz
         -- padding bytes
         + Padding sz typ
         -- field size
         + SizeOf typ
         )

-- | Struct alignment
type family StructAlignment (fs :: [*]) where
   StructAlignment fs = StructAlignment' fs 1

-- | Struct alignment
type family StructAlignment' (fs :: [*]) (a :: Nat) where
   StructAlignment' '[]                    a = a
   StructAlignment' (Field name typ ': fs) a =
      StructAlignment' fs
         (IfNat (a <=? Alignment typ) (Alignment typ) a)

-- | Compute the required padding between a and b to respect b's alignment
type family RequiredPadding a b where
   RequiredPadding a b = Padding (SizeOf a) b

-- | Compute the required padding between the size sz and b to respect b's alignment
type family Padding (sz :: Nat) b where
   Padding sz b = PaddingEx (Modulo sz (Alignment b)) (Alignment b)

type family PaddingEx (m :: Nat) (a :: Nat) where
   PaddingEx 0 a = 0
   PaddingEx m a = a - m

-- newtype AsStruct a = AsStruct a
-- 
-- instance Struct a => Storable (AsStruct a) a where
--    type SizeOf    (AsStruct a) = SizeOfStruct (ExtractFields a)
--    type Alignment (AsStruct a) = StructAlignment (ExtractFields a) 1
--    peekPtr = peekStruct
--    pokePtr = undefined


-- | Peek fields into structure pointed by p
-- data PeekFields p = PeekFields p
-- 
-- instance forall name t ts f p l.
--       ( Storable t t
--       , f ~ IO (t -> ts)
--       , t ~ LayoutPathType l (LayoutPath '[LayoutSymbol name])
--       , PtrLike p
--       , KnownNat (LayoutPathOffset l (LayoutPath '[LayoutSymbol name]))
--       ) => Apply (PeekFields (p l)) (Field name t, f) (IO ts)
--    where
--       apply (PeekFields p) (_, f) = f <*> peek (p --> LayoutSymbol @name)
-- 
-- -- | Peek fields of a struct and put them in a record
-- peekFields :: forall l fs p a f.
--    ( HFoldl' (PeekFields (p l)) (IO f) fs (IO a)
--    , PtrLike p
--    ) => f -> p a -> IO a
-- peekFields f p = hFoldl' (PeekFields p') f' (undefined :: HList fs)
--    where
--       p' :: p l
--       p' = castPtr p
-- 
--       f' :: IO f
--       f' = return f
-- 
-- type family StructConstructor a where
--    StructConstructor a = StructConstructor' (ExtractFields a) a
-- 
-- type family StructConstructor' fs a where
--    StructConstructor' '[]                  a = a
--    StructConstructor' (Field name t ': fs) a = t -> StructConstructor' fs a

newtype Struct a = Struct a

instance MemoryLayout (Struct e) where
   type SizeOf    (Struct e) = SizeOfStruct    (ExtractFields e)
   type Alignment (Struct e) = StructAlignment (ExtractFields e)

type instance LayoutPathType (Struct a) (LayoutPath (p ': ps))
   = LayoutPathType (StructLayout (ExtractFields a)) (LayoutPath (p ': ps))

type instance LayoutPathOffset (Struct a) (LayoutPath (p ': ps))
   = LayoutPathOffset (StructLayout (ExtractFields a)) (LayoutPath (p ': ps))

instance
      ( Generic a
      , GStorable (Struct a) (Rep a)
      ) => Storable (Struct a) a
   where
      peekPtr p   = fmap to (gcPeek p)
      pokePtr p x = gcPoke p (from x)
   

-- class AsStruct a where
--    -- | Constructor to use to build the struct
--    makeStruct :: StructConstructor a
-- 
-- -- | Peek a struct
-- peekStruct :: forall p a fs f l.
--    ( AsStruct a
--    , PtrLike p
--    , fs ~ ExtractFields a
--    , f ~ StructConstructor a
--    , l ~ StructLayout fs
--    , HFoldl' (PeekFields (p l)) (IO f) fs (IO a)
--    ) => p a -> IO a
-- peekStruct p = peekFields @(StructLayout fs) @fs (makeStruct @a) p
-- 
-- type family ConstructorStruct f where
--    ConstructorStruct f = ConstructorStruct' f SymNames
-- 
-- type family ConstructorStruct' f ns where
--    ConstructorStruct' (a -> b) (n ': ns) = Field n a ': ConstructorStruct' b ns
--    ConstructorStruct' b        (n ': ns) = '[] -- constructed data type
-- 
-- type family ConstructorRet f where
--    ConstructorRet (a -> b) = ConstructorRet b
--    ConstructorRet b        = b
-- 
-- -- | Symbol names
-- -- TODO: generate
-- type SymNames
--    = '[ "_0", "_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9"
--       , "_10", "_11", "_12", "_13", "_14", "_15", "_16", "_17", "_18", "_19"
--       , "_20", "_21", "_22", "_23", "_24", "_25", "_26", "_27", "_28", "_29"
--       ]
-- 
-- -- | Peek a struct from a constructor
-- peekStructCons :: forall p fs a f l.
--    ( PtrLike p
--    , a ~ ConstructorRet f
--    , fs ~ ConstructorStruct f
--    , l ~ StructLayout fs
--    , HFoldl' (PeekFields (p l)) (IO f) fs (IO a)
--    ) => f -> p a -> IO a
-- peekStructCons f p = peekFields @(StructLayout fs) @fs f (castPtr p)
-- 
-- TODO:
-- pokeStruct :: PtrLike p => p a -> IO a

-- | Temporarily store 'c' into memory and pass the pointer to 'f'
withStruct :: forall b c.
   ( Storable (Struct c) c
   , KnownNat (SizeOf (Struct c))
   ) => c -> (Ptr (Struct c) -> IO b) -> IO b
withStruct = with

-- | Temporarily allocate enough bytes to hold a 'a'
allocaStruct :: forall a b.
   ( KnownNat (SizeOf (Struct a))
   ) => (Ptr (Struct a) -> IO b) -> IO b
allocaStruct = alloca

-- | Peek specialized for structs
peekStruct :: (PtrLike p, Storable (Struct a) a) => p (Struct a) -> IO a
{-# INLINE peekStruct #-}
peekStruct = peek
