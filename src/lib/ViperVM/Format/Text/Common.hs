{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Text common
module ViperVM.Format.Text.Common
   ( TextEncoding (..)
   , TextGlyphSet (..)
   , TextSequence (..)
   , Text (..)
   , TextCase (..)
   )
where

import ViperVM.Format.Binary.Buffer
import ViperVM.Format.Binary.Word

-- | A text glyphset describes the glyphs that can be encoded. 
class TextGlyphSet a where

   -- | Some glyphs support case operations (upper, lower, etc.)
   type SupportCase a :: Bool


-- | Text encoding
--
-- A text encoding is a serial protocol: it is a sequence of CodeType.
class TextEncoding a where

   -- | Type of each code in the sequence
   type CodeType a :: *

   -- | Some codes are combined
   type HasCombinations a :: Bool


-- | A sequence of codes
class TextSequence s where

   type Elem s :: *

   seqMap :: (Elem s -> Elem s) -> s -> s

   
instance TextSequence [Word8] where
   type Elem [Word8] = Word8
   seqMap = fmap

instance TextSequence Buffer where
   type Elem Buffer = Word8
   seqMap = bufferMap


newtype Text g e s = Text s deriving (Show,Eq)


-- | Text whose case can be switched (upper/lower)
class TextCase a where
   -- | Switch to upper case
   toUpper :: a -> a

   -- | Switch to lower case
   toLower :: a -> a

