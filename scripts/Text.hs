import ViperVM.Format.Binary.Buffer
import ViperVM.Format.Binary.Word
import ViperVM.Format.Text.Common
import ViperVM.Format.Text.ASCII

ex1, ex1',ex1'' :: Text ASCII ASCII [Word8]
ex1 = Text [50,58,59,67,87,96,29]

ex2,ex2',ex2'' :: Text ASCII ASCII Buffer
ex2 = Text (bufferPackByteList [50,58,59,67,87,96,29])

ex1' = toLower ex1
ex1'' = toUpper ex1

ex2' = toLower ex2
ex2'' = toUpper ex2

-- ex3 :: Text Unicode EncUTF8 StructBuffer
-- ex3 = Text (bufferPackByteList [50,58,59,67,87,96,29])
-- 
-- ex4 :: Text Unicode EncUTF32 StructList
-- ex4 = Text "hello WORLD!!"
-- 
-- ex5 :: Text Unicode EncUTF32 StructList
-- ex5 = Text "baﬄe"
