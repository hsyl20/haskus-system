{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Store an Enum in the given backing word type
module ViperVM.Format.Binary.Enum
   ( EnumField
   , CEnum (..)
   , fromEnumField
   , toEnumField
   )
where

import ViperVM.Format.Binary.Layout
import ViperVM.Format.Binary.Storable
import ViperVM.Format.Binary.Ptr

-----------------------------------------------------------------------------
-- EnumField b a: directly store the value of enum "a" as a "b"
-----------------------------------------------------------------------------

-- | Store enum 'a' as a 'b'
newtype EnumField b a = EnumField a deriving (Show,Eq)

instance MemoryLayout (EnumField b a) where
   type SizeOf    (EnumField b a) = SizeOf b
   type Alignment (EnumField b a) = Alignment b
   

instance
      ( Integral b
      , Storable b b
      , CEnum a
      ) => Storable (EnumField b a) a
   where
      peekPtr p   = toCEnum <$> peekPtr (castPtr p :: Ptr b)
      pokePtr p v = pokePtr (castPtr p :: Ptr b) (fromCEnum v)

type XX = WithLayout (EnumField Int) Int

-- | Read an enum field
fromEnumField :: EnumField b a -> a
fromEnumField (EnumField a) = a

{-# INLINE fromEnumField #-}

-- | Create an enum field
toEnumField :: a -> EnumField b a
toEnumField = EnumField

{-# INLINE toEnumField #-}

-----------------------------------------------------------------------------
-- Extended Enum
-----------------------------------------------------------------------------

-- | By default, use fromEnum/toEnum to convert from/to an Integral.
--
-- But it can be overloaded to perform transformation before using
-- fromEnum/toEnum. E.g. if values are shifted by 1 compared to Enum values,
-- define fromCEnum = (+1) . fromIntegral . fromEnum
--
class CEnum a where
   fromCEnum         :: Integral b => a -> b
   default fromCEnum :: (Enum a, Integral b) => a -> b
   fromCEnum         = fromIntegral . fromEnum

   toCEnum         :: Integral b => b -> a
   default toCEnum :: (Enum a, Integral b) => b -> a
   toCEnum         = toEnum . fromIntegral

