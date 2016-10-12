{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Dynamically sized vector
module ViperVM.Format.Binary.Array
   ( Array
   , arrayCount
   , arrayFromList
   , arrayToList
   )
where

import ViperVM.Format.Binary.Buffer
import ViperVM.Format.Binary.Storable
import ViperVM.Format.Binary.Layout
import ViperVM.Utils.Types

-- | Array of t's
newtype Array t = Array Buffer

instance
   ( Show t
   , FixedStorable t t
   , KnownNat (SizeOf t)
   ) => Show (Array t) where
   show a = "fromList " ++ show (arrayToList a)

-- | Count elements in the array
arrayCount :: forall t.
   ( FixedStorable t t
   , KnownNat (SizeOf t)
   ) => Array t -> Word
arrayCount (Array b) = bufferSize b `div` n
   where
      n = layoutSizeOf (undefined :: t)

-- | Create an array from a list of values
arrayFromList ::
   ( FixedStorable t t
   , KnownNat (SizeOf t)
   ) => [t] -> Array t
arrayFromList xs = Array (bufferPackStorableList xs)

-- | Create a list from an array
arrayToList :: forall t.
   ( FixedStorable t t
   , KnownNat (SizeOf t)
   ) => Array t -> [t]
arrayToList a@(Array b) = bufferPeekStorableAt b <$> offs
   where
      offs = fmap (* sz) [0..n-1]
      sz   = layoutSizeOf (undefined :: t)
      n    = arrayCount a
