{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

-- | Union (as in C)
--
-- Unions are storable and can contain any storable data.
-- 
-- Use 'fromUnion' to read a alternative:
--
-- @
-- {-# LANGUAGE DataKinds #-}
--
-- getUnion :: IO (Union '[Word16 Word32 Word64])
-- getUnion = ...
--
-- test = do
--    u <- getUnion
--
--    -- to get one of the member
--    let v = fromUnion u :: Word16
--    let v = fromUnion u :: Word32
--    let v = fromUnion u :: Word64
--
--    -- This won't compile (Word8 is not a member of the union)
--    let v = fromUnion u :: Word8
-- @
--
-- Use 'toUnion' to create a new union:
-- @
--
-- let
--    u2 :: Union '[Word32, Vector 4 Word8]
--    u2 = toUnion (0x12345678 :: Word32)
-- @
module ViperVM.Format.Binary.Union
   ( Union
   , fromUnion
   , toUnion
   , toUnionZero
   , unionCast
   )
where

import ViperVM.Utils.Memory (memCopy, memSet)
import ViperVM.Utils.Types
import ViperVM.Utils.Types.List hiding (Union)
import ViperVM.Utils.HList
import ViperVM.Format.Binary.Storable
import ViperVM.Format.Binary.Layout
import ViperVM.Format.Binary.Ptr

import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (when)


-- TODO: rewrite rules
-- poke p (toUnion x) = poke (castPtr p) x
--
-- (fromUnion <$> peek p) :: IO a  = peek (castPtr p) :: IO a



-- | An union 
--
-- We use a list of types as a parameter.
--
-- The union is just a pointer to a buffer containing the value(s). The size of
-- the buffer is implicitly known from the types in the list.
newtype Union (x :: [*]) = Union (ForeignPtr ()) deriving (Show)

-- | Retrieve a union member from its type
fromUnion :: (Storable a a, IsMember a l ~ 'True) => Union l -> a
fromUnion (Union fp) = unsafePerformIO $ withForeignPtr fp (peek' . castPtr)

-- | Create a new union from one of the union types
toUnion :: forall a l.
   ( Storable (Union l) (Union l)
   , Storable a a
   , IsMember a l ~ 'True
   , KnownNat (SizeOf (Union l))
   , KnownNat (SizeOf a)
   ) => a -> Union l
toUnion = toUnion' False

-- | Like 'toUnion' but set the remaining bytes to 0
toUnionZero :: forall a l.
   ( Storable (Union l) (Union l)
   , Storable a a
   , IsMember a l ~ 'True
   , KnownNat (SizeOf (Union l))
   , KnownNat (SizeOf a)
   ) => a -> Union l
toUnionZero = toUnion' True


-- | Create a new union from one of the union types
toUnion' :: forall a l.
   ( Storable (Union l) (Union l)
   , Storable a a
   , IsMember a l ~ 'True
   , KnownNat (SizeOf (Union l))
   , KnownNat (SizeOf a)
   ) => Bool -> a -> Union l
toUnion' zero v = unsafePerformIO $ do
   let sz = fromIntegral (sizeOf @(Union l))
   fp <- mallocForeignPtrBytes sz
   withForeignPtr fp $ \p -> do
      -- set bytes after the object to 0
      when zero $ do
         let psz = fromIntegral (sizeOf @a)
         memSet (p `indexPtr` psz) (fromIntegral (sz - psz)) 0
      poke' (castPtr p) v
   return $ Union fp

-- | Convert two storable values
-- TODO: remove this
unionCast :: forall a b.
   ( Storable a a
   , Storable b b
   , Member a '[a,b]
   , Member b '[a,b]
   , KnownNat (SizeOf a)
   , KnownNat (SizeOf (Union '[a,b]))
   , KnownNat (Alignment (Union '[a,b]))
   ) => a -> b
unionCast a = fromUnion u
   where
      u :: Union '[a,b]
      u = toUnion a

type family MapSizeOf fs where
   MapSizeOf '[]       = '[]
   MapSizeOf (x ': xs) = SizeOf x ': MapSizeOf xs

type family MapAlignment fs where
   MapAlignment '[]       = '[]
   MapAlignment (x ': xs) = Alignment x ': MapAlignment xs

instance MemoryLayout (Union fs) where
   type SizeOf (Union fs)    = Max (MapSizeOf fs)
   type Alignment (Union fs) = Max (MapAlignment fs)

instance forall fs.
      ( KnownNat (Max (MapSizeOf fs))
      , KnownNat (Max (MapAlignment fs))
      )
      => Storable (Union fs) (Union fs)
   where
      peekPtr ptr = do
         let sz = fromIntegral $ sizeOf @(Union fs)
         fp <- mallocForeignPtrBytes sz
         withForeignPtr fp $ \p -> 
            memCopy p (castPtr ptr) (fromIntegral sz)
         return (Union fp)

      pokePtr ptr (Union fp) = do
         let sz = sizeOf @(Union fs)
         withForeignPtr fp $ \p ->
            memCopy (castPtr ptr) p (fromIntegral sz)
