{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

-- | Unicode encoding
module ViperVM.Format.Text.Unicode
   ( Unicode
   , UTF8
   , UTF16
   , UTF32
   )
where

import ViperVM.Format.Text.Common
import ViperVM.Format.Binary.Word

-- | Unicode code set
data Unicode

-- | UTF-8 encoding
data UTF8

-- | UTF-16 encoding
data UTF16

-- | UTF-32 encoding
data UTF32


instance TextGlyphSet Unicode where
   type SupportCase Unicode = 'True

instance TextEncoding UTF8 where
   type CodeType UTF8        = Word8
   type HasCombinations UTF8 = 'True

instance TextEncoding UTF16 where
   type CodeType UTF16        = Word16
   type HasCombinations UTF16 = 'True

instance TextEncoding UTF32 where
   type CodeType UTF32        = Word32
   type HasCombinations UTF32 = 'True
