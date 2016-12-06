{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Memory utilities
module ViperVM.Utils.Memory
   ( memCopy
   , memSet
   , allocaArrays
   , peekArrays
   , pokeArrays
   , withArrays
   , withMaybeOrNull
   , wordBytes
   )
where

import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Ptr
import ViperVM.Format.Binary.Storable
import ViperVM.Utils.Flow
import ViperVM.Utils.Types
import System.IO.Unsafe

-- | Copy memory
memCopy :: MonadIO m => Ptr a -> Ptr b -> Word64 -> m ()
{-# INLINE memCopy #-}
memCopy dest src size = liftIO (void (memcpy dest src size))

-- | memcpy
foreign import ccall unsafe memcpy  :: Ptr a -> Ptr b -> Word64 -> IO (Ptr c)



-- | Set memory
memSet :: MonadIO m => Ptr a -> Word64 -> Word8 -> m ()
{-# INLINE memSet #-}
memSet dest size fill = liftIO (void (memset dest fill size))

-- | memset
foreign import ccall unsafe memset  :: Ptr a -> Word8 -> Word64 -> IO (Ptr c)


-- | Allocate several arrays
allocaArrays :: (MonadInIO m, IsStorable s, Integral a) => [a] -> ([Ptr s] -> m b) -> m b
allocaArrays sizes f = go [] sizes
   where
      go as []     = f (reverse as)
      go as (x:xs) = allocaArray (fromIntegral x) $ \a -> go (a:as) xs

-- | Peek several arrays
peekArrays :: (MonadIO m, IsStorable s, Integral a) => [a] -> [Ptr s] -> m [[s]]
peekArrays szs ptrs = mapM f (szs `zip` ptrs)
   where
      f (sz,p) = peekArray (fromIntegral sz) p

-- | Poke several arrays
pokeArrays :: (MonadIO m, IsStorable s) => [Ptr s] -> [[s]] -> m ()
pokeArrays ptrs vs = mapM_ f (ptrs `zip` vs)
   where
      f = uncurry pokeArray

-- | Allocate several arrays
withArrays :: (MonadInIO m, IsStorable s) => [[s]] -> ([Ptr s] -> m b) -> m b
withArrays vs f = go [] vs
   where
      go as []     = f (reverse as)
      go as (x:xs) = withArray x $ \a -> go (a:as) xs

-- | Execute f with a pointer to 'a' or NULL
withMaybeOrNull ::
   ( IsStorable a
   , MonadInIO m
   ) => Maybe a -> (Ptr a -> m b) -> m b
withMaybeOrNull s f = case s of
   Nothing -> f nullPtr
   Just x  -> with x f

-- | Get bytes in host-endianness order
wordBytes :: forall a. (IsStorable a) => a -> [Word8]
{-# INLINE wordBytes #-}
wordBytes x = unsafePerformIO $
   with x $ \p -> mapM (peekByteOff (castPtr p)) [0..natValue @(SizeOf a) - 1]
