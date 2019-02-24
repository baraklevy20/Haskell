{-# LANGUAGE LambdaCase #-}

module CPU.Emulate where

import CPU.CPU (CPU, Error)
import qualified CPU.CPU as CPU
import qualified Data.ByteString as BS
import qualified System.Random as Rand
import qualified Data.Vector as V
import qualified Data.Word as W (Word8, Word16)
import Control.Monad ((>=>))
import Lens.Micro ((&))
import qualified Lens.Micro as Lens (set, over)
import qualified Lens.Micro.Mtl as Lens (view)
import Data.Bits
import qualified CPU.Bits as Bits
import Numeric (showHex)

fontSet :: V.Vector W.Word8
fontSet = V.fromList [
   0xF0, 0x90, 0x90, 0x90, 0xF0 -- 0
  ,0x20, 0x60, 0x20, 0x20, 0x70 -- 1
  ,0xF0, 0x10, 0xF0, 0x80, 0xF0 -- 2
  ,0xF0, 0x10, 0xF0, 0x10, 0xF0 -- 3
  ,0x90, 0x90, 0xF0, 0x10, 0x10 -- 4
  ,0xF0, 0x80, 0xF0, 0x10, 0xF0 -- 5
  ,0xF0, 0x80, 0xF0, 0x90, 0xF0 -- 6
  ,0xF0, 0x10, 0x20, 0x40, 0x40 -- 7
  ,0xF0, 0x90, 0xF0, 0x90, 0xF0 -- 8
  ,0xF0, 0x90, 0xF0, 0x10, 0xF0 -- 9
  ,0xF0, 0x90, 0xF0, 0x90, 0x90 -- A
  ,0xE0, 0x90, 0xE0, 0x90, 0xE0 -- B
  ,0xF0, 0x80, 0x80, 0x80, 0xF0 -- C
  ,0xE0, 0x90, 0x90, 0x90, 0xE0 -- D
  ,0xF0, 0x80, 0xF0, 0x80, 0xF0 -- E
  ,0xF0, 0x80, 0xF0, 0x80, 0x80] -- F

type Instruction = (CPU -> CPU)

loadGameAndFonts :: BS.ByteString -> Either String CPU
loadGameAndFonts =
  loadGame >=> pure . loadFonts

loadFonts :: CPU -> CPU
loadFonts = Lens.over CPU.memory (\mem -> V.update_ mem (V.enumFromN 0 (V.length fontSet)) fontSet)

loadGame :: BS.ByteString -> Either String CPU
loadGame buffer = Right $ cpu & Lens.over CPU.memory (\mem -> V.update_ mem (V.enumFromN 0x200 (length unpackedBuffer)) (V.fromList unpackedBuffer))
         where cpu = CPU.initCPU
               unpackedBuffer = BS.unpack buffer

updateTimers :: CPU -> CPU
updateTimers cpu = cpu & Lens.over CPU.delayTimer (\x -> if x > 0 then x - 1 else 0) &
                         Lens.over CPU.soundTimer (\x -> if x > 0 then x - 1 else 0)

cleanSoundTimer :: CPU -> CPU
cleanSoundTimer cpu = cpu & Lens.set CPU.soundTimer 0

emulateCycle :: CPU -> Either String CPU
emulateCycle cpu = execute cpu $ decode cpu $ fetch cpu

fetch :: CPU -> W.Word16
fetch cpu = (shift (fromIntegral $ Lens.view CPU.memory cpu V.! CPU.getPC cpu) 8) .|.
            (fromIntegral $ Lens.view CPU.memory cpu V.! (CPU.getPC cpu + 1))

decode :: CPU -> W.Word16 -> Either String Instruction
decode cpu opcode = case findOpcode opcode of
                        Just instruction -> Right instruction
                        Nothing -> Left $ "Command not found: " ++ (showHex opcode "")

execute :: CPU -> Either String Instruction -> Either String CPU
execute _ (Left error) = Left error
execute cpu (Right instruction) = Right $ instruction cpu

increasePC :: CPU -> CPU
increasePC = Lens.over CPU.pc (+2)

findOpcode :: W.Word16 -> Maybe Instruction
findOpcode opcode = case Bits.splitOpcode opcode of
    (6, x, n1, n2) -> Just $ increasePC >>= (pure . (setRegister x $ Bits.twoWords4toWord8 n1 n2))
    _ -> Nothing


setRegister :: W.Word8 -> W.Word8 -> Instruction
setRegister x val = Lens.over CPU.vx (V.// [(fromIntegral x, val)])
