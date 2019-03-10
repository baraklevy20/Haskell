-- Bits Utilities

module CPU.Bits where
import Data.Word
import Data.Bits

showHex16 :: Word16 -> String
showHex16 x = show x

--splits the opcode (a Word16) to a tuple of 4 Word8s
splitOpcode :: Word16 -> (Word8, Word8, Word8, Word8)
splitOpcode n =
  (fromIntegral $ rotateL (n .&. 0xF000) 4
  ,fromIntegral $ rotateR (n .&. 0x0F00) 8
  ,fromIntegral $ rotateR (n .&. 0x00F0) 4
  ,fromIntegral (n .&. 0x000F)
  )


--merges 2 words of Word8 to a single Word8
twoWords4toWord8 :: Word8 -> Word8 -> Word8
twoWords4toWord8 w1 w2 = (shift (fromIntegral (w1 .&. 0xF)) 4) .|. (fromIntegral (w2 .&. 0xF))

--merges 3 words of Word8 to a single Word16
threeWords4toWord16 :: Word8 -> Word8 -> Word8 -> Word16
threeWords4toWord16 w1 w2 w3 = (shift (fromIntegral $ w1) 8) .|.
                             (shift (fromIntegral $ w2) 4) .|.
                             (fromIntegral w3)
