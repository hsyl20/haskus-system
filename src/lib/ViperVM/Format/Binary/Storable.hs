{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}

-- | Storable class
module ViperVM.Format.Binary.Storable
   ( Storable (..)
   , GStorable (..)
   , sizeOf
   , alignment
   , MemoryLayout (..)
   -- * Peek/Poke
   , peek
   , poke
   , peek'
   , poke'
   , peekByteOff
   , pokeByteOff
   , peekByteOff'
   , pokeByteOff'
   , peekElemOff
   , pokeElemOff
   , peekElemOff'
   , pokeElemOff'
   , peekShow
   , unsafePeekShow
   , peekArray
   , pokeArray
   , peekArray'
   , pokeArray'
   , malloc
   , mallocPoke
   , mallocDup
   , copy
   , allocaBytes
   , alloca
   , with
   , withMaybeOrNull
   )
where

import qualified Foreign.Storable      as FS
import qualified Foreign.Marshal.Alloc as Ptr

import ViperVM.Format.Binary.Layout
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Ptr
import ViperVM.Utils.Types
import ViperVM.Utils.Types.Generics
import ViperVM.Utils.Memory
import System.IO.Unsafe

------------------------------------------------
-- Storable instance deriving from Generic
------------------------------------------------

-- | Store a field 'a' with layout 'l'
class GStorable l a where
   gcPeek      :: Ptr l -> IO (a x)
   gcPoke      :: Ptr l -> a x -> IO ()

-- constructor without argument
instance GStorable l U1 where
   gcPeek _    = return U1
   gcPoke _ _  = return ()

-- passthrough data type metadata
instance (GStorable l a) => GStorable l (M1 D c a) where
  gcPeek p           = M1 <$> gcPeek p
  gcPoke p (M1 x)    = gcPoke p x

-- passthrough constructor metadata (we only support a single constructor
-- because there is no instance for (:+:))
instance (GStorable l a) => GStorable l (M1 C c a) where
  gcPeek p           = M1 <$> gcPeek p
  gcPoke p (M1 x)    = gcPoke p x

-- passthrough field product binary operator
instance (GStorable l a, GStorable l b) => GStorable l (a :*: b) where
   gcPeek p           = (:*:) <$> gcPeek p <*> gcPeek p
   gcPoke p (a :*: b) = gcPoke p a >> gcPoke p b

-- peek field from the field selector
instance
   ( Storable t t
   , KnownNat (LayoutPathOffset l (LayoutPath '[LayoutSymbol name]))
   , t ~ LayoutPathType l (LayoutPath '[LayoutSymbol name])
   ) => GStorable l (M1 S ('MetaSel ('Just name) u v w) (K1 R t))
   where
      gcPeek p             = (M1 . K1) <$> peekPtr (p --> LayoutSymbol @name)
      gcPoke p (M1 (K1 x)) = pokePtr (p --> LayoutSymbol @name) x

------------------------------------------------
-- Storable instance
------------------------------------------------

-- | 'e' is a storable data that can be read/written from/into memory with the
-- representation 'r'
class Storable r e | r -> e where
   -- | Peek (read) a value from a memory address
   peekPtr :: Ptr r -> IO e

   default peekPtr :: (Generic e, GStorable r (Rep e)) => Ptr r -> IO e
   peekPtr p = fmap to $ gcPeek p

   -- | Poke (write) a value at the given memory address
   pokePtr :: Ptr r -> e -> IO ()

   default pokePtr :: (Generic e, GStorable r (Rep e)) => Ptr r -> e -> IO ()
   pokePtr p x = gcPoke p $ from x

-- | Get statically known size
sizeOf :: forall r. KnownNat (SizeOf r) => Word
sizeOf = natValue @(SizeOf r)

-- | Get statically known alignment
alignment :: forall r. KnownNat (Alignment r) => Word
alignment = natValue @(Alignment r)

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

instance Storable Word8 Word8 where
   peekPtr = FS.peek
   pokePtr = FS.poke

instance Storable Word16 Word16 where
   peekPtr = FS.peek
   pokePtr = FS.poke

instance Storable Word32 Word32 where
   peekPtr = FS.peek
   pokePtr = FS.poke

instance Storable Word64 Word64 where
   peekPtr = FS.peek
   pokePtr = FS.poke

instance Storable Int8 Int8 where
   peekPtr = FS.peek
   pokePtr = FS.poke

instance Storable Int16 Int16 where
   peekPtr = FS.peek
   pokePtr = FS.poke

instance Storable Int32 Int32 where
   peekPtr = FS.peek
   pokePtr = FS.poke

instance Storable Int64 Int64 where
   peekPtr = FS.peek
   pokePtr = FS.poke


---------------------------------------------------------------------
-- Peek / Poke
---------------------------------------------------------------------

peek :: (PtrLike p, Storable a b) => p a -> IO b
{-# INLINE peek #-}
peek p = withPtr p peekPtr

poke :: (PtrLike p, Storable a b) => p a -> b -> IO ()
{-# INLINE poke #-}
poke p b = withPtr p (`pokePtr` b)

-- | Peek specialized for primitives
peek' :: (PtrLike p, Storable a a) => p a -> IO a
{-# INLINE peek' #-}
peek' = peek

-- | Poke specialized for primitives
poke' :: (PtrLike p, Storable a a) => p a -> a -> IO ()
{-# INLINE poke' #-}
poke' = poke

peekByteOff :: (PtrLike p, Storable a b) => p a -> Int -> IO b
{-# INLINE peekByteOff #-}
peekByteOff p n = withPtr (p `indexPtr` n) peekPtr

pokeByteOff :: (PtrLike p, Storable a b) => p a -> Int -> b -> IO ()
{-# INLINE pokeByteOff #-}
pokeByteOff p n b = withPtr (p `indexPtr` n) (`pokePtr` b)

peekByteOff' :: (PtrLike p, Storable a a) => p a -> Int -> IO a
{-# INLINE peekByteOff' #-}
peekByteOff' = peekByteOff

pokeByteOff' :: (PtrLike p, Storable a a) => p a -> Int -> a -> IO ()
{-# INLINE pokeByteOff' #-}
pokeByteOff' = pokeByteOff

peekElemOff :: forall p a b.
   ( PtrLike p
   , Storable a b
   , KnownNat (SizeOf a)
   ) => p a -> Int -> IO b
{-# INLINE peekElemOff #-}
peekElemOff p n = withPtr (p `indexPtr` off) peekPtr
   where off = n * fromIntegral (sizeOf @a)

pokeElemOff :: forall p a b.
   ( PtrLike p
   , Storable a b
   , KnownNat (SizeOf a)
   ) => p a -> Int -> b -> IO ()
{-# INLINE pokeElemOff #-}
pokeElemOff p n b = withPtr (p `indexPtr` off) (`pokePtr` b)
   where off = n * fromIntegral (sizeOf @a)

peekElemOff' :: forall p a.
   ( PtrLike p
   , Storable a a
   , KnownNat (SizeOf a)
   ) => p a -> Int -> IO a
{-# INLINE peekElemOff' #-}
peekElemOff' = peekElemOff

pokeElemOff' :: forall p a.
   ( PtrLike p
   , Storable a a
   , KnownNat (SizeOf a)
   ) => p a -> Int -> a -> IO ()
{-# INLINE pokeElemOff' #-}
pokeElemOff' = pokeElemOff

-- | Show the element which is pointed at
peekShow :: (Show b, PtrLike p, Storable a b) => p a -> IO String
{-# INLINE peekShow #-}
peekShow p = show <$> peek p

-- | Show the element which is pointed at
unsafePeekShow :: (Show b, PtrLike p, Storable a b) => p a -> String
{-# NOINLINE unsafePeekShow #-}
unsafePeekShow = unsafePerformIO . peekShow

-- | Peek 'n' elements
peekArray ::
   ( PtrLike p
   , Storable a b
   , KnownNat (SizeOf a)
   ) => Word -> p a -> IO [b]
{-# INLINE peekArray #-}
peekArray n p = go [] (fromIntegral n)
   where
      go xs 0 = return xs
      go xs i = do
         x <- peekElemOff p i
         go (x:xs) (i-1)

-- | Poke all the elements in the list
pokeArray ::
   ( PtrLike p
   , Storable a b
   , KnownNat (SizeOf a)
   ) => p a -> [b] -> IO ()
{-# INLINE pokeArray #-}
pokeArray p bs = go 0 bs
   where
      go _ []     = return ()
      go i (x:xs) = do
         pokeElemOff p i x 
         go (i+1) xs

-- | Peek 'n' elements
peekArray' ::
   ( PtrLike p
   , Storable a a
   , KnownNat (SizeOf a)
   ) => Word -> p a -> IO [a]
{-# INLINE peekArray' #-}
peekArray' = peekArray

-- | Poke all the elements in the list
pokeArray' ::
   ( PtrLike p
   , Storable a a
   , KnownNat (SizeOf a)
   ) => p a -> [a] -> IO ()
{-# INLINE pokeArray' #-}
pokeArray' = pokeArray

-- | Allocate memory able to contain 'a'
malloc :: forall a p.
   ( PtrLike p
   , KnownNat (SizeOf a)
   ) => IO (p a)
malloc = mallocBytes (natValue @(SizeOf a))

-- | Allocate memory able to contain 'b' and put it in it
mallocPoke :: forall a b p.
   ( PtrLike p
   , Storable a b
   , KnownNat (SizeOf a)
   ) => b -> IO (p a)
mallocPoke b = do
   p <- malloc
   poke p b
   return p

-- | Temporarily allocate some bytes
allocaBytes :: Word -> (Ptr a -> IO b) -> IO b
allocaBytes n f = Ptr.allocaBytes (fromIntegral n) f

-- | Temporarily allocate enough bytes to hold a 'a'
alloca :: forall a b.
   ( KnownNat (SizeOf a)
   ) => (Ptr a -> IO b) -> IO b
alloca = allocaBytes (sizeOf @a)

-- | Temporarily store 'c' into memory and pass the pointer to 'f'
with :: forall a b c.
   ( Storable a c
   , KnownNat (SizeOf a)
   ) => c -> (Ptr a -> IO b) -> IO b
with c f = alloca $ \p -> do
   poke p c
   f p

-- | Execute f with a pointer to 'a' or NULL
withMaybeOrNull :: forall r e b.
   ( Storable r e
   , KnownNat (SizeOf r)
   ) => Maybe e -> (Ptr r -> IO b) -> IO b
withMaybeOrNull s f = case s of
   Nothing -> f nullPtr
   Just x  -> with x f

-- | Copy from a pointer to another
copy :: forall a p1 p2.
   ( PtrLike p1
   , PtrLike p2
   , KnownNat (SizeOf a)
   ) => p1 a -> p2 a -> IO ()
copy fp1 fp2 = 
   withPtr fp1 $ \p1 ->
      withPtr fp2 $ \p2 ->
         memCopy p2 p1 (natValue @(SizeOf a))

-- | Allocate memory able to contain 'a' and duplicate it in it
mallocDup :: forall a p1 p2.
   ( PtrLike p1
   , PtrLike p2
   , KnownNat (SizeOf a)
   ) => p1 a -> IO (p2 a)
mallocDup fp1 = do
   fp2 <- mallocBytes (natValue @(SizeOf a))
   fp1 `copy` fp2
   return fp2

instance Storable (r e) e => Storable (WithLayout r e) (WithLayout r e)
   where
      peekPtr p = WithLayout <$> peekPtr (castPtr p :: Ptr (r e))
      pokePtr p (WithLayout e) = pokePtr (castPtr p :: Ptr (r e)) e

