{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module ViperVM.Format.NewText
   ( Text (..)
   -- * Glyph sets
   , ASCII
   , Unicode
   -- * Encodings
   , EncByte
   , EncUTF8
   , EncUTF16
   , EncUTF32
   -- * Data structures
   , StructList
   , StructBuffer
   -- * Operations
   , Reversable (..)
   , CaseSwitchable (..)
   -- * Test
   , ex1
   , ex2
   , ex3
   , ex4
   , ex5
   )
where

import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Buffer
import Data.Char

-- | What is a Text? Depending on the use case, the answer is different.
--
--   * Unicode tries to cover the whole set of existing natural languages.
--   Comparison requires normalization; reversing the letters is not equivalent
--   to reversing the code points (cf diacritics); some letters support "case"
--   while others don't; some precomposed characters require special care. 
--
--   Unicode is similar to a light LaTeX: there are control characters to force
--   joining or non-joining; it tries to handle hyphenation generically; it
--   supports bidirectional text.
--
--   Many operations are locale sensitive:
--    * sorting
--    * searching: options are locale sensitive too:
--       * case-sensitivity
--       * how to consider special characters (accentuated letters, Ç, etc.)
--       * what is searching with kanji-like glyphs?
--   
--   It is both too high-level for common operations (comparison, etc.) that are
--   easy and efficient with ASCII-like encoding and too low-level for most
--   other operations requiring complex formatting and locale sensitivity.
--
--   * ASCII-like character sets only support a few characters. Characters have
--   a fixed size and most operations are trivial (e.g., using conversion
--   tables).
--
--   * POSIX FilePath can be composed of any byte except \0 and '/'
--
--   * Various ASCII subsets can be used too (e.g., in ISO 9660)
--
-- Depending on the use, the data structure used to hold the text should be different:
--    * Comparison, search, indexing
--    * Concatenation (builder)
--    * Insertion/deletion (text editor)
--    * FFI (e.g., with legacy UTF-16 or ASCII libraries)
--    * Streaming
--
-- Existing String/Text types:
--   * Data.Text: a UTF-16 encoded Unicode in a ByteString
--       - wastes space for most ASCII-like strings (2-bytes per character)
--       - variable-length encoding implying O(n) operations
--       - cache-friendly and small overhead thanks to ByteString
--
--   * Data.Text.Lazy: lazy variant of Text with chunks of data
--
--   * String type: [Char]
--       - wastes lot of space (both for Char and because of the linked list)
--       - list operations are mostly wrong in the general case:
--          - reverse: reverse the code points (cf diacritics)
--          - map toUpper: some characters such as 'ffl' become 3-characters in upper-case ("FFL")
--
-- Encodings often contain control-characters that are in fact part of a
-- terminal protocol (e.g., 0x00 to 0x1F in ASCII), used for rendering
-- algorithms (e.g., Bidirectional and joining in Unicode), etc.
--
-- A generic Text type?
--    We need a taxonomy

data Text glyphSet glyphEnc textStruct = Text (MakeStruct glyphSet glyphEnc textStruct)

instance (Show (MakeStruct g e s)) => Show (Text g e s) where
   show (Text l) = show l

-- | Glyph sets
data ASCII
data UpperIdentifier -- [A-Z0-9]
data Unicode

-- | Encodings
data EncByte
data EncUTF8
data EncUTF16
data EncUTF32

-- | Data structures
data StructList
data StructBuffer

class Glyph a where
   -- | Some glyphs can be combined
   -- It prohibits naïve reverse operations
   type HasCombinations a :: Bool

   -- | Case-switching makes sense for the character set
   type HasCaseSwitchable a :: Bool

instance Glyph ASCII where
   type HasCombinations   ASCII = 'False
   type HasCaseSwitchable ASCII = 'True

instance Glyph UpperIdentifier where
   type HasCombinations   UpperIdentifier = 'False
   type HasCaseSwitchable UpperIdentifier = 'False

instance Glyph Unicode where
   type HasCombinations   Unicode = 'True
   type HasCaseSwitchable Unicode = 'True


type family   MakeStruct g e s

type instance MakeStruct ASCII   EncByte  StructList   = [Word8]
type instance MakeStruct ASCII   EncByte  StructBuffer = Buffer

type instance MakeStruct UpperIdentifier EncByte StructList   = [Word8]
type instance MakeStruct UpperIdentifier EncByte StructBuffer = Buffer

type instance MakeStruct Unicode EncUTF8  StructList   = [Word8]
type instance MakeStruct Unicode EncUTF16 StructList   = [Word16]
type instance MakeStruct Unicode EncUTF32 StructList   = String -- Just for the test
type instance MakeStruct Unicode EncUTF8  StructBuffer = Buffer
type instance MakeStruct Unicode EncUTF16 StructBuffer = Buffer
type instance MakeStruct Unicode EncUTF32 StructBuffer = Buffer

----------------------------------------------------------
-- Reverse
----------------------------------------------------------

-- | Is a sequence of code-points reversable?
type IsReversable g e =
   ( HasCombinations g ~ 'False
   )

class Reversable a where
   -- | Reverse glyphs
   reverseGlyphs :: a -> a

instance forall g e a.
      ( IsReversable g e
      , MakeStruct g e StructList ~ [a]
      ) => Reversable (Text g e StructList)
   where
      reverseGlyphs (Text l) = Text (reverse l)

instance forall g e.
      ( IsReversable g e
      , MakeStruct g e StructBuffer ~ Buffer
      ) => Reversable (Text g e StructBuffer)
   where
      reverseGlyphs (Text l) = Text (bufferReverse l)

----------------------------------------------------------
-- Case-switch
----------------------------------------------------------

class CaseSwitchable a where
   -- | Switch to upper-case
   uppercase :: a -> a

   -- | Switch to lower-case
   lowercase :: a -> a

asciiUpper :: Word8 -> Word8
asciiUpper x = if x >= 97 && x <= 122 then x - 22 else x

asciiLower :: Word8 -> Word8
asciiLower x = if x >= 65 && x <= 90 then x + 22 else x

instance CaseSwitchable (Text ASCII EncByte StructList) where
   uppercase (Text l) = Text (map asciiUpper l)
   lowercase (Text l) = Text (map asciiLower l)

instance CaseSwitchable (Text ASCII EncByte StructBuffer) where
   uppercase (Text l) = Text (bufferMap asciiUpper l)
   lowercase (Text l) = Text (bufferMap asciiLower l)

-- | FIXME: maybe we shouldn't provide the instance at all
instance CaseSwitchable (Text UpperIdentifier e s) where
   uppercase = id
   lowercase = id

-- | FIXME: wrong instance, just for testing purpose
instance CaseSwitchable (Text Unicode EncUTF32 StructList) where
   uppercase (Text l) = Text (map toUpper l)
   lowercase (Text l) = Text (map toLower l)

ex1 :: Text ASCII EncByte StructList
ex1 = Text [50,58,59,67,87,96,29]

ex2 :: Text ASCII EncByte StructBuffer
ex2 = Text (bufferPackByteList [50,58,59,67,87,96,29])

ex3 :: Text Unicode EncUTF8 StructBuffer
ex3 = Text (bufferPackByteList [50,58,59,67,87,96,29])

ex4 :: Text Unicode EncUTF32 StructList
ex4 = Text "hello WORLD!!"

ex5 :: Text Unicode EncUTF32 StructList
ex5 = Text "baﬄe"
