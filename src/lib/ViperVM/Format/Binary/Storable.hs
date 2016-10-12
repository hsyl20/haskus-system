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

-- | Storable class
module ViperVM.Format.Binary.Storable
   ( FixedStorable (..)
   , peekFrom
   , pokeFrom
   , peek
   , poke
   , peekByteOffFrom
   , pokeByteOffFrom
   , peekByteOff
   , pokeByteOff
   , peekElemOffFrom
   , pokeElemOffFrom
   , peekElemOff
   , pokeElemOff
   , peekShow
   , unsafePeekShow
   , peekArrayFrom
   , pokeArrayFrom
   , peekArray
   , pokeArray
   , malloc
   , mallocPoke
   , mallocDup
   , copy
   , allocaBytes
   , alloca
   , with
   -- * Explicit layout
   , WithLayout (..)
   )
where

import qualified Foreign.Storable      as FS
import qualified Foreign.Marshal.Alloc as Ptr

import ViperVM.Format.Binary.Layout
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Ptr
import ViperVM.Utils.Types
import ViperVM.Utils.Memory
import System.IO.Unsafe

-- | Data that can be read into (or written from) a Haskell data-type `b` from a
-- pointer with layout `a`.
class MemoryLayout a => FixedStorable a b | a -> b where
   -- | Peek (read) a value from a memory address
   fixedPeek :: Ptr a -> IO b

   -- | Poke (write) a value at the given memory address
   fixedPoke :: Ptr a -> b -> IO ()

peekFrom :: (PtrLike p, FixedStorable a b) => p a -> IO b
{-# INLINE peekFrom #-}
peekFrom p = withPtr p fixedPeek

pokeFrom :: (PtrLike p, FixedStorable a b) => p a -> b -> IO ()
{-# INLINE pokeFrom #-}
pokeFrom p b = withPtr p (`fixedPoke` b)

peek :: (PtrLike p, FixedStorable a a) => p a -> IO a
{-# INLINE peek #-}
peek = peekFrom

poke :: (PtrLike p, FixedStorable a a) => p a -> a -> IO ()
{-# INLINE poke #-}
poke = pokeFrom

peekByteOffFrom :: (PtrLike p, FixedStorable a b) => p a -> Int -> IO b
{-# INLINE peekByteOffFrom #-}
peekByteOffFrom p n = withPtr (p `indexPtr` n) fixedPeek

pokeByteOffFrom :: (PtrLike p, FixedStorable a b) => p a -> Int -> b -> IO ()
{-# INLINE pokeByteOffFrom #-}
pokeByteOffFrom p n b = withPtr (p `indexPtr` n) (`fixedPoke` b)

peekByteOff :: (PtrLike p, FixedStorable a a) => p a -> Int -> IO a
{-# INLINE peekByteOff #-}
peekByteOff = peekByteOffFrom

pokeByteOff :: (PtrLike p, FixedStorable a a) => p a -> Int -> a -> IO ()
{-# INLINE pokeByteOff #-}
pokeByteOff = pokeByteOffFrom

peekElemOffFrom :: forall p a b.
   ( PtrLike p
   , FixedStorable a b
   , KnownNat (SizeOf a)
   ) => p a -> Int -> IO b
{-# INLINE peekElemOffFrom #-}
peekElemOffFrom p n = withPtr (p `indexPtr` off) fixedPeek
   where off = n * fromIntegral (sizeOf @a)

pokeElemOffFrom :: forall p a b.
   ( PtrLike p
   , FixedStorable a b
   , KnownNat (SizeOf a)
   ) => p a -> Int -> b -> IO ()
{-# INLINE pokeElemOffFrom #-}
pokeElemOffFrom p n b = withPtr (p `indexPtr` off) (`fixedPoke` b)
   where off = n * fromIntegral (sizeOf @a)

peekElemOff :: forall p a.
   ( PtrLike p
   , FixedStorable a a
   , KnownNat (SizeOf a)
   ) => p a -> Int -> IO a
{-# INLINE peekElemOff #-}
peekElemOff = peekElemOffFrom

pokeElemOff :: forall p a.
   ( PtrLike p
   , FixedStorable a a
   , KnownNat (SizeOf a)
   ) => p a -> Int -> a -> IO ()
{-# INLINE pokeElemOff #-}
pokeElemOff = pokeElemOffFrom

-- | Show the element which is pointed at
peekShow :: (Show b, PtrLike p, FixedStorable a b) => p a -> IO String
{-# INLINE peekShow #-}
peekShow p = show <$> peekFrom p

-- | Show the element which is pointed at
unsafePeekShow :: (Show b, PtrLike p, FixedStorable a b) => p a -> String
{-# NOINLINE unsafePeekShow #-}
unsafePeekShow = unsafePerformIO . peekShow

-- | Peek 'n' elements
peekArrayFrom ::
   ( PtrLike p
   , FixedStorable a b
   , KnownNat (SizeOf a)
   ) => Word -> p a -> IO [b]
{-# INLINE peekArrayFrom #-}
peekArrayFrom n p = go [] (fromIntegral n)
   where
      go xs 0 = return xs
      go xs i = do
         x <- peekElemOffFrom p i
         go (x:xs) (i-1)

-- | Poke all the elements in the list
pokeArrayFrom ::
   ( PtrLike p
   , FixedStorable a b
   , KnownNat (SizeOf a)
   ) => p a -> [b] -> IO ()
{-# INLINE pokeArrayFrom #-}
pokeArrayFrom p bs = go 0 bs
   where
      go _ []     = return ()
      go i (x:xs) = do
         pokeElemOffFrom p i x 
         go (i+1) xs

-- | Peek 'n' elements
peekArray ::
   ( PtrLike p
   , FixedStorable a a
   , KnownNat (SizeOf a)
   ) => Word -> p a -> IO [a]
{-# INLINE peekArray #-}
peekArray = peekArrayFrom

-- | Poke all the elements in the list
pokeArray ::
   ( PtrLike p
   , FixedStorable a a
   , KnownNat (SizeOf a)
   ) => p a -> [a] -> IO ()
{-# INLINE pokeArray #-}
pokeArray = pokeArrayFrom

-- | Allocate memory able to contain 'a'
malloc :: forall a p.
   ( PtrLike p
   , KnownNat (SizeOf a)
   ) => IO (p a)
malloc = mallocBytes (natValue @(SizeOf a))

-- | Allocate memory able to contain 'b' and put it in it
mallocPoke :: forall a b p.
   ( PtrLike p
   , FixedStorable a b
   , KnownNat (SizeOf a)
   ) => b -> IO (p a)
mallocPoke b = do
   p <- malloc
   pokeFrom p b
   return p

-- | Temporarily allocate some bytes
allocaBytes :: Word -> (Ptr a -> IO b) -> IO b
allocaBytes n f = Ptr.allocaBytes (fromIntegral n) f

-- | Temporarily allocate enough bytes to hold a 'a'
alloca :: forall a b c.
   ( FixedStorable a c
   , KnownNat (SizeOf a)
   ) => (Ptr a -> IO b) -> IO b
alloca = allocaBytes (sizeOf @a)

-- | Temporarily store 'c' into memory and pass the pointer to 'f'
with :: forall a b c.
   ( FixedStorable a c
   , KnownNat (SizeOf a)
   ) => c -> (Ptr a -> IO b) -> IO b
with c f = alloca $ \p -> do
   pokeFrom p c
   f p

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
mallocDup :: forall a b p1 p2.
   ( PtrLike p1
   , PtrLike p2
   , KnownNat (SizeOf a)
   ) => p1 a -> IO (p2 a)
mallocDup fp1 = do
   fp2 <- mallocBytes (natValue @(SizeOf a))
   fp1 `copy` fp2
   return fp2

instance FixedStorable Word8 Word8 where
   fixedPeek = FS.peek
   fixedPoke = FS.poke

instance FixedStorable Word16 Word16 where
   fixedPeek = FS.peek
   fixedPoke = FS.poke

instance FixedStorable Word32 Word32 where
   fixedPeek = FS.peek
   fixedPoke = FS.poke

instance FixedStorable Word64 Word64 where
   fixedPeek = FS.peek
   fixedPoke = FS.poke

instance FixedStorable Int8 Int8 where
   fixedPeek = FS.peek
   fixedPoke = FS.poke

instance FixedStorable Int16 Int16 where
   fixedPeek = FS.peek
   fixedPoke = FS.poke

instance FixedStorable Int32 Int32 where
   fixedPeek = FS.peek
   fixedPoke = FS.poke

instance FixedStorable Int64 Int64 where
   fixedPeek = FS.peek
   fixedPoke = FS.poke

-- | Explicit layout
newtype WithLayout r e = WithLayout e deriving (Show,Eq,Ord)

type instance LayoutPathType (WithLayout r e) (LayoutPath (p ': ps))
   = LayoutPathType r (LayoutPath (p ': ps))

type instance LayoutPathOffset (WithLayout r e) (LayoutPath (p ': ps))
   = LayoutPathOffset r (LayoutPath (p ': ps))

instance MemoryLayout r => MemoryLayout (WithLayout r e) where
   type SizeOf    (WithLayout r e) = SizeOf r
   type Alignment (WithLayout r e) = Alignment r

instance FixedStorable r e => FixedStorable (WithLayout r e) (WithLayout r e)
   where
      fixedPeek p = WithLayout <$> fixedPeek (castPtr p :: Ptr r)

      fixedPoke p (WithLayout e) = fixedPoke (castPtr p :: Ptr r) e
