{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ASCII encoding
module ViperVM.Format.Text.ASCII
   ( ASCII
   , asciiToUpper
   , asciiToLower
   )
where

import ViperVM.Format.Text.Common
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Bits


-- | ASCII glyph set and encoding
data ASCII

instance TextGlyphSet ASCII where
   type SupportCase ASCII = 'True

instance TextEncoding ASCII where
   type CodeType ASCII        = Word8
   type HasCombinations ASCII = 'False

-- | Convert a character to upper case
asciiToUpper :: CodeType ASCII -> CodeType ASCII
asciiToUpper x
   | x > 0x60 && x < 0x7B = x .&. 0xDF
   | otherwise            = x

-- | Convert a character to lower case
asciiToLower :: CodeType ASCII -> CodeType ASCII
asciiToLower x
   | x > 0x40 && x < 0x5B = x .|. 0x20
   | otherwise            = x


instance
   ( TextSequence s
   , Elem s ~ CodeType ASCII
   ) => TextCase (Text ASCII ASCII s)
   where
      toUpper (Text s) = Text (seqMap asciiToUpper s)
      toLower (Text s) = Text (seqMap asciiToLower s)
