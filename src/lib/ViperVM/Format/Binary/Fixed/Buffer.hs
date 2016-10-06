{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Data
--
-- We want data to be as cheap as a C pointer (or as close as possible), hence
-- we decorate a pointer with a type describing the layout of the memory at the
-- pointer position. This is similar to C types such as 'int *", "struct A *",
-- "char [25]", struct A [10][5]", etc.
--
-- We want to support slices: "struct A[5] ys = (struct A[5])&xs[10];" but with
-- more safety.
--
-- We want to support dynamic type check for dynamically bounded vectors.
module ViperVM.Format.Binary.Fixed.Buffer
   ( Data (..)
   , Vector
   , Slice
   , Record
   , Field
   , DynVector (..)
   , DynData (..)
   , PathDynIndex
   , readData
   , sliceData
   )
where

import ViperVM.Format.Binary.Ptr
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Storable
import ViperVM.Format.Binary.Fixed.LayoutPtr
import ViperVM.Utils.Types
import ViperVM.Utils.HList
import ViperVM.Utils.Memory

-------------------------------------------------------------------------
-- Layouts
-- ~~~~~~~
--
-- A layout describes memory regions in a buffer. A layout is built as a
-- hierarchy of layouts (as in C).
-------------------------------------------------------------------------

-- | A vector of 'n' elements with type 'e'
data Vector e (n :: Nat)

-- | A slice ('offset' and 'size' expressed in the sliced vector unit 'e')
data Slice (offset :: Nat) (size :: Nat) v

-- | Record with named fields
data Record (fields :: [*])

-- | Record field
data Field (name :: Symbol) (t :: *)


-- | A path to reach an element
data PathDynIndex

-- | A data with the given layout
newtype Data l = Data (ForeignPtr Word8)

-- | A data with the given layout and the given offset
data SubData l = SubData {-# UNPACK #-} !(ForeignPtr Word8)    -- pointer
                         {-# UNPACK #-} !Word                  -- offset

-- | Dynamically checked vector
data DynVector e = DynVector Word

-- | A dynamically checked data with the given layout
data DynData l = DynData {-# UNPACK #-} !(ForeignPtr Word8)    -- ^ Pointer
                         ![Word]                               -- ^ Dynamic vector sizes

-- | TODO: replace with TypeError
data OutOfBound

-- | Compute the type at a given path for the given layout
type family ComputePathType layout path where
   ComputePathType (Slice off sz (Vector e n)) ps =
      If (off + sz <=? n)
         (ComputePathType (Vector e sz) ps)
         OutOfBound

   ComputePathType (Slice off sz (DynVector e)) ps = ComputePathType (Vector e sz) ps

   ComputePathType (Vector e n)              '[]      = Data (Vector e n)
   ComputePathType (Record fs)               '[]      = Data (Record fs)
   ComputePathType e                         '[]      = e

   ComputePathType (Vector e n)  (PathDynIndex ': ps) = ComputePathType e ps
   ComputePathType (DynVector e) (PathIndex i ': ps)  = ComputePathType e ps
   ComputePathType (DynVector e) (PathDynIndex ': ps) = ComputePathType e ps
   ComputePathType (Vector e n)  (PathIndex i ': ps)  =
      If ((i+1) <=? n)
         (ComputePathType e ps)
         OutOfBound

   ComputePathType (Record fs) (PathSymbol s ': ps) = ComputePathType (RecordFieldType fs s) ps


type family RecordFieldType fs (s :: Symbol) where
   RecordFieldType (Field name t ': fs) name = t
   RecordFieldType (Field name t ': fs) s    = RecordFieldType fs s

-- | Compute the offset at a given path for the given layout
type family ComputePathOffset off layout path where
   ComputePathOffset off e             '[]                  = off

   ComputePathOffset off (DynVector e) (PathIndex i ': ps)  = 
      ComputePathOffset (off + SizeOf (ComputePathType e '[]) * i) e ps
   ComputePathOffset off (Vector e n)  (PathIndex i ': ps)  =
      -- FIXME: use TypeError with Nat kind
      -- IfNat ((i+1) <=? n)
         (ComputePathOffset (off + SizeOf (ComputePathType e '[]) * i) e ps)
      --   OutOfBound

   ComputePathOffset off (Slice off2 sz (Vector e n)) ps =
      -- FIXME: use TypeError with Nat kind
      -- IfNat (off2 + sz <=? n)
         (ComputePathOffset (off + off2) (Vector e sz) ps)
      --   OutOfBound
   ComputePathOffset off (Record fs) (PathSymbol s ': ps) = 
      ComputePathOffset (off + RecordFieldOffset s fs 0) (RecordFieldType fs s) ps

-- | Offset of a record field (with padding taken into account)
type family RecordFieldOffset (name :: Symbol) (fs :: [*]) (sz :: Nat) where
   RecordFieldOffset name (Field name typ ': fs) sz = sz + Padding sz typ
   RecordFieldOffset name (Field xx typ ': fs) sz =
      RecordFieldOffset name fs
         (sz + Padding sz typ + SizeOf typ)

instance
   ( KnownNat (SizeOf (ComputePathType e '[]) * n)
   ) => FixedStorable (Data (Vector e n))
   where
      type SizeOf (Data (Vector e n))    = SizeOf (ComputePathType e '[]) * n
      type Alignment (Data (Vector e n)) = Alignment (ComputePathType e '[])

      fixedPeek ptr = do
         let n = fromIntegral (natVal (Proxy :: Proxy (SizeOf (Data (Vector e n)))))
         fp <- mallocForeignPtrBytes n
         withForeignPtr fp $ \ptr2 ->
            memCopy ptr ptr2 (fromIntegral n)
         return (Data fp)

      fixedPoke = undefined


readData :: forall l p.
   ( FixedStorable (ComputePathType l p)
   , KnownNat (ComputePathOffset 0 l p)
   ) => Path p -> Data l -> IO (ComputePathType l p)
readData _ (Data fp) = withForeignPtr fp (fixedPeek . castPtr . (`plusPtr` off))
   where
      off = fromIntegral (natVal (Proxy :: Proxy (ComputePathOffset 0 l p)))


type family RemoveData p where
   RemoveData (Data x) = x
   RemoveData x        = x


sliceData :: forall l p.
   ( FixedStorable (ComputePathType l p)
   , KnownNat (ComputePathOffset 0 l p)
   ) => Path p -> Data l -> SubData (RemoveData (ComputePathType l p))
sliceData _ (Data fp) = SubData fp off
   where
      off = fromIntegral (natVal (Proxy :: Proxy (ComputePathOffset 0 l p)))
   

data ReadDyn = ReadDyn

instance forall e n.
   ( KnownNat (SizeOf e)
   , KnownNat n
   ) => ApplyAB ReadDyn (PathDynIndex, ([Word],[Word],Data (Vector e n), Word))
                                       ([Word],[Word],Data e, Word)
   where
      applyAB _ (_, (pathIdxs,layoutSizes,_,offset)) =
            if i < n
               then (tail pathIdxs, layoutSizes, undefined, offset + i*se)
               else error ("Index out of bound: " ++ show i ++ " while bound is " ++ show n)
         where
            i  = head pathIdxs
            n  = fromIntegral (natVal (Proxy :: Proxy n))
            se = fromIntegral (natVal (Proxy :: Proxy (SizeOf e)))

instance forall e n.
   ( KnownNat (SizeOf e)
   , KnownNat n
   ) => ApplyAB ReadDyn (PathDynIndex, ([Word],[Word],Data (DynVector e),Word))
                                       ([Word],[Word],Data e,Word)
   where
      applyAB _ (_, (pathIdxs,layoutSizes,_,offset)) =
            if i < n
               then (tail pathIdxs, tail layoutSizes, undefined, offset + i*se)
               else error ("Index out of bound: " ++ show i ++ " while bound is " ++ show n)
         where
            i  = head pathIdxs
            n  = head layoutSizes
            se = fromIntegral (natVal (Proxy :: Proxy (SizeOf e)))

-- readDynData :: forall l p e ps.
--    ( FixedStorable (ComputePathType l p)
--    , HFoldr' ReadDyn ([Word],[Word],Data l,Word) ps ([Word],[Word],Data e,Word)
--    , e ~ ComputePathType l p
--    ) => Path p -> DynData l -> IO (ComputePathType l p)
-- readDynData (Path ps) (DynData fp szs) = withForeignPtr fp (fixedPeek . castPtr . (`plusPtr` off))
--    where
--       initFold     = (ps,szs,(undefined :: Data l),(0 :: Word))
--       (_,_,_,off') = hFoldr' ReadDyn initFold (undefined :: HList ps) :: ([Word],[Word],Data e, Word)
--       off          = fromIntegral off'
-- 
-- | Read a Data with a dynamic path (runtime indexes)
-- readDynData' :: DynPath p -> Data l -> IO (ComputePathType l p)
--
-- TODO: use a type-class for these 3 cases
--
-- -- | Don't check bounds
-- unsafeReadDynData'' :: DynPath p -> DynData l -> IO (ComputePathType l p)
