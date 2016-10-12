{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

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
   , Storable t t
   , KnownNat (SizeOf t)
   ) => Show (Array t) where
   show a = "fromList " ++ show (arrayToList a)

-- | Count elements in the array
arrayCount :: forall t.
   ( Storable t t
   , KnownNat (SizeOf t)
   ) => Array t -> Word
arrayCount (Array b) = bufferSize b `div` n
   where
      n = sizeOf @t

-- | Create an array from a list of values
arrayFromList ::
   ( Storable t t
   , KnownNat (SizeOf t)
   ) => [t] -> Array t
arrayFromList xs = Array (bufferPackStorableList xs)

-- | Create a list from an array
arrayToList :: forall t.
   ( Storable t t
   , KnownNat (SizeOf t)
   ) => Array t -> [t]
arrayToList a@(Array b) = bufferPeekStorableAt b <$> offs
   where
      offs = fmap (* sz) [0..n-1]
      sz   = sizeOf @t
      n    = arrayCount a
