{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ViperVM.Format.Binary.Vector
   ( Vector (..)
   , empty
   , take
   , toList
   , fromList
   , fromFilledList
   , fromFilledListZ
   , replicate
   )
where

import Prelude hiding (take,replicate)

import ViperVM.Format.Binary.Ptr
import ViperVM.Format.Binary.Layout
import ViperVM.Format.Binary.Storable
import qualified ViperVM.Format.Binary.Layout.Vector as VL
import ViperVM.Format.Binary.Layout.Vector (VectorLayout)
import ViperVM.Utils.Types
import qualified ViperVM.Utils.List as List
import System.IO.Unsafe

-- | A vector
newtype Vector (n :: Nat) e = Vector (FinalizedPtr (VectorLayout n e))

instance
   ( Storable a a
   , Show a
   , KnownNat n
   , KnownNat (SizeOf a)
   ) => Show (Vector n a) where
   show v = "fromList " ++ show (toList v)

instance
   ( KnownNat (n * SizeOf e)
   ) => Storable (VectorLayout n e) (Vector n e)
   where
   type SizeOf    (VectorLayout n e) = n * SizeOf e
   type Alignment (VectorLayout n e) = Alignment e
   peekPtr p              = Vector <$> mallocDup (castPtr p)
   pokePtr p2 (Vector p1) = castPtr p1 `copy` p2

instance
      ( KnownNat (n * SizeOf e)
      ) => Storable (Vector n e) (Vector n e)
   where
   type SizeOf    (Vector n e) = SizeOf    (VectorLayout n e)
   type Alignment (Vector n e) = Alignment (VectorLayout n e)
   peekPtr p              = Vector <$> mallocDup (castPtr p)
   pokePtr p2 (Vector p1) = castPtr p1 `copy` p2

-- | Empty vector
empty :: Vector 0 e
{-# INLINE empty #-}
empty = Vector nullPtr

-- | Take the 'i' first elements in the vector
--
-- O(1) (the underlying vector is shared)
take :: forall i n e.
   ( CmpNat i (n+1) ~ 'LT
   ) => Vector n e -> Vector i e
{-# INLINE take #-}
take (Vector p) = Vector (VL.vectorTake p (Proxy :: Proxy i))

-- | Convert a vector into a list
toList ::
   ( KnownNat n
   , KnownNat (SizeOf e)
   , Storable e e
   ) => Vector n e -> [e]
{-# NOINLINE toList #-}
toList (Vector p) = unsafePerformIO $ VL.vectorPeekList p

-- | Convert a list into a vector if the length matches
fromList :: forall e n.
   ( KnownNat n
   , KnownNat (SizeOf e)
   , Storable e e
   ) => [e] -> Maybe (Vector n e)
{-# NOINLINE fromList #-}
fromList xs 
   | List.null xs && natValue' @n == 0 = Just (Vector nullPtr)
   | List.checkLength (natValue @n) xs = unsafePerformIO $ do
         p <- mallocBytes (natValue @n * sizeOf @e)
         VL.unsafeVectorPokeList p xs -- we have already checked the length
         return (Just (Vector p))
   | otherwise                         = Nothing

-- | Convert a list into a vector and fill missing element with 'e'
fromFilledList :: forall e n.
   ( KnownNat n
   , KnownNat (SizeOf e)
   , Storable e e
   ) => e -> [e] -> Vector n e
{-# NOINLINE fromFilledList #-}
fromFilledList e es 
   | natValue' @n == 0  = Vector nullPtr
   | otherwise          = unsafePerformIO $ do
         p <- mallocBytes (natValue @n * sizeOf @e)
         VL.vectorPokeFilledList p e es
         return (Vector p)

-- | Convert a list into a vector and fill missing element with 'e'
fromFilledListZ :: forall e n.
   ( KnownNat n
   , KnownNat (SizeOf e)
   , Storable e e
   ) => e -> [e] -> Vector n e
{-# NOINLINE fromFilledListZ #-}
fromFilledListZ e es 
   | natValue' @n == 0  = Vector nullPtr
   | otherwise          = unsafePerformIO $ do
         p <- mallocBytes (natValue @n * sizeOf @e)
         VL.vectorPokeFilledListZ p e es
         return (Vector p)

-- | Replicate a value to fill a vector
replicate :: forall n e.
   ( KnownNat n
   , KnownNat (SizeOf e)
   , Storable e e
   ) => e -> Vector n e
{-# NOINLINE replicate #-}
replicate e
   | natValue' @n == 0 = Vector nullPtr
   | otherwise         = unsafePerformIO $ do
         p <- mallocBytes (natValue @n * sizeOf @e)
         VL.vectorFill p e
         return (Vector p)
