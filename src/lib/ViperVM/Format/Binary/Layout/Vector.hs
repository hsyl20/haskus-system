{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

-- | Vector
module ViperVM.Format.Binary.Layout.Vector
   ( VectorLayout
   , vectorSlice
   , vectorTake
   , vectorDrop
   , vectorPeekList
   , vectorPokeList
   , unsafeVectorPokeList
   , vectorPokeFilledList
   , vectorPokeFilledListZ
   , vectorFill
   )
where

import ViperVM.Format.Binary.Layout
import ViperVM.Format.Binary.Ptr
import ViperVM.Format.Binary.Storable
import ViperVM.Utils.Types
import qualified ViperVM.Utils.List as List

-- | A vector layout of 'n' elements with type 'e'
data VectorLayout (n :: Nat) e

type instance LayoutPathType (VectorLayout n e) (LayoutPath (LayoutIndex i ': ps))  =
   If ((i+1) <=? n)
      (LayoutPathType e (LayoutPath ps))
      (TypeError (Text "Vector index out of bound: " :<>: ShowType i))

type instance LayoutPathOffset (VectorLayout n e) (LayoutPath (LayoutIndex i ': ps))  =
   -- FIXME: (GHC8) use TypeError with Nat kind
   -- IfNat ((i+1) <=? n)
      (SizeOf e * i + (LayoutPathOffset e (LayoutPath ps)))
   --   OutOfBound

instance MemoryLayout (VectorLayout n e) where
   type SizeOf    (VectorLayout n e) = n * SizeOf e
   type Alignment (VectorLayout n e) = Alignment e

-- | Offset of the i-th element in a stored vector
type family ElemOffset a n where
   ElemOffset a n = n * (SizeOf a)


-- | Slice a vector
vectorSlice :: forall n e i m p.
   ( CmpNat (i + m) (n + 1) ~ 'LT -- check that subrange is valid
   , KnownNat (SizeOf e * i)
   , PtrLike p
   ) => p (VectorLayout n e) -> Proxy i -> Proxy m -> p (VectorLayout m e)
{-# INLINE vectorSlice #-}
vectorSlice p _ _ = castPtr (p -#> (LayoutIndex :: LayoutIndex i))


-- | Take the 'm' first elements
vectorTake :: forall n e m p.
   ( CmpNat m (n + 1) ~ 'LT -- check that subrange is valid
   , PtrLike p
   ) => p (VectorLayout n e) -> Proxy m -> p (VectorLayout m e)
{-# INLINE vectorTake #-}
vectorTake p = vectorSlice p (Proxy :: Proxy 0)


-- | Drop the 'i' first elements
vectorDrop :: forall n e i m p.
   ( CmpNat (i + m) (n + 1) ~ 'LT -- check that subrange is valid
   , m ~ (n - i)
   , KnownNat (SizeOf e * i)
   , PtrLike p
   ) => p (VectorLayout n e) -> Proxy i -> p (VectorLayout m e)
{-# INLINE vectorDrop #-}
vectorDrop p i = vectorSlice p i (Proxy :: Proxy m)

-- | Convert a vector into a list
vectorPeekList :: forall n e p.
   ( KnownNat n
   , FixedStorable e e
   , KnownNat (SizeOf e)
   , PtrLike p
   ) => p (VectorLayout n e) -> IO [e]
{-# INLINE vectorPeekList #-}
vectorPeekList p
   | n == 0    = return []
   | otherwise = peekArray n (castPtr p :: p e)
   where
      n = natValue @n

-- | Convert a list into a vector if the number of elements matches
vectorPokeList :: forall n e p.
   ( KnownNat n
   , FixedStorable e e
   , KnownNat (SizeOf e)
   , PtrLike p
   ) => p (VectorLayout n e) -> [e] -> IO ()
{-# INLINE vectorPokeList #-}
vectorPokeList p vs = 
      if List.checkLength (natValue @n) vs
         then pokeArray (castPtr p) vs
         else error "vectorFromList: invalid list size"

-- | Convert a list into a vector (don't check list size)
unsafeVectorPokeList :: forall n e p.
   ( KnownNat n
   , FixedStorable e e
   , KnownNat (SizeOf e)
   , PtrLike p
   ) => p (VectorLayout n e) -> [e] -> IO ()
{-# INLINE unsafeVectorPokeList #-}
unsafeVectorPokeList p vs = pokeArray (castPtr p) vs

-- | Take at most n element from the list, then use z
vectorPokeFilledList :: forall n e p.
   ( KnownNat n
   , FixedStorable e e
   , KnownNat (SizeOf e)
   , PtrLike p
   ) => p (VectorLayout n e) -> e -> [e] -> IO ()
{-# INLINE vectorPokeFilledList #-}
vectorPokeFilledList p z v = pokeArray (castPtr p) vs
   where
      vs = List.take n (v ++ repeat z)
      n  = natValue @n

-- | Take at most (n-1) element from the list, then use z
vectorPokeFilledListZ :: forall n e p.
   ( KnownNat n
   , FixedStorable e e
   , KnownNat (SizeOf e)
   , PtrLike p
   ) => p (VectorLayout n e) -> e -> [e] -> IO ()
{-# INLINE vectorPokeFilledListZ #-}
vectorPokeFilledListZ p z v = vectorPokeFilledList p z vs
   where
      vs = List.take (natValue @n - 1) v

-- | Create a vector by replicating a value
vectorFill :: forall n e p.
   ( KnownNat n
   , FixedStorable e e
   , KnownNat (SizeOf e)
   , PtrLike p
   ) => p (VectorLayout n e) -> e -> IO ()
{-# INLINE vectorFill #-}
vectorFill p z = vectorPokeFilledList p z []

-- TODO:
-- 
-- data StoreVector = StoreVector -- Store a vector at the right offset
-- 
-- instance forall (n :: Nat) v a r s.
--    ( v ~ Vector n a
--    , r ~ IO (Ptr a)
--    , KnownNat n
--    , KnownNat (SizeOf a)
--    , s ~ ElemOffset a n
--    , KnownNat s
--    , FixedStorable a a
--    ) => Apply StoreVector (v, IO (Ptr a)) r where
--       apply _ (v, getP) = do
--          p <- getP
--          let
--             vsz = fromIntegral (natVal (Proxy :: Proxy n))
--             p'  = p `indexPtr` (-1 * vsz * sizeOf (undefined :: a))
--          poke (castPtr p') v 
--          return p'
-- 
-- type family WholeSize fs :: Nat where
--    WholeSize '[]                 = 0
--    WholeSize (Vector n s ': xs)  = n + WholeSize xs
-- 
-- -- | Concat several vectors into a single one
-- concat :: forall l (n :: Nat) a .
--    ( n ~ WholeSize l
--    , KnownNat n
--    , FixedStorable a a
--    , HFoldr StoreVector (IO (Ptr a)) l (IO (Ptr a))
--    )
--    => HList l -> Vector n a
-- concat vs = unsafePerformIO $ do
--    let sz = layoutSizeOf (undefined :: a) * fromIntegral (natVal (Proxy :: Proxy n))
--    p <- mallocBytes sz :: IO (Ptr ())
--    _ <- hFoldr StoreVector (return (castPtr p `indexPtr` sz) :: IO (Ptr a)) vs :: IO (Ptr a)
--    Vector <$> bufferUnsafePackPtr (fromIntegral sz) p
