-- Bits Utilities

module CPU.Bits where
import Data.Word
import Data.Bits

showHex16 :: Word16 -> String
showHex16 x = show x

splitOpcode :: Word16 -> (Word8, Word8, Word8, Word8)
--splitOpcode opcode = (
--    (shift intOpcode (-12)) .&. 0xF,
--    (shift intOpcode (-8)) .&. 0xF,
--    (shift intOpcode (-4)) .&. 0xF,
--    intOpcode .&. 0xF)
--    where intOpcode = fromIntegral opcode
splitOpcode n =
  (fromIntegral $ rotateL (n .&. 0xF000) 4
  ,fromIntegral $ rotateR (n .&. 0x0F00) 8
  ,fromIntegral $ rotateR (n .&. 0x00F0) 4
  ,fromIntegral (n .&. 0x000F)
  )


twoWords4toWord8 :: Word8 -> Word8 -> Word8
twoWords4toWord8 w1 w2 = (shift (fromIntegral $ w1) 4) .|. (fromIntegral w2)

threeWords4toWord16 :: Word8 -> Word8 -> Word8 -> Word16
threeWords4toWord16 w1 w2 w3 = (shift (fromIntegral $ w1) 8) .|.
                             (shift (fromIntegral $ w2) 4) .|.
                             (fromIntegral w3)
