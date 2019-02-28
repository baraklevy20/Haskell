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
                        --Nothing -> Left $ show( ((snd popResult) & (Lens.set CPU.pc $ (fst popResult))))
                          -- where popResult = CPU.popFromStack cpu
                        --Nothing -> Left $ show(cpu)
                        Nothing -> Left $ "Command not found: " ++ (showHex opcode "")

execute :: CPU -> Either String Instruction -> Either String CPU
execute _ (Left error) = Left error
execute cpu (Right instruction) = Right $ instruction cpu

increasePC :: CPU -> CPU
increasePC = Lens.over CPU.pc (+2)

findOpcode :: W.Word16 -> Maybe Instruction
findOpcode opcode = case Bits.splitOpcode opcode of
    (0x0, 0x0, 0xe, 0x0) -> Just $ (Lens.over CPU.graphics (V.map (const False))) >>= pure.increasePC
    (0x0, 0x0, 0xe, 0xe) -> Just $ (returnFromSubroutine) >>= pure.increasePC
    (0x1, n1, n2, n3) -> Just $ (jumpToAddress $ Bits.threeWords4toWord16 n1 n2 n3)
    (0x2, n1, n2, n3) -> Just $ (callSubroutine $ Bits.threeWords4toWord16 n1 n2 n3) >>= pure.increasePC
    (0x3, x, n1, n2) -> Just $ (skipInstructionIf (\cpu -> CPU.getRegister x cpu == Bits.twoWords4toWord8 n1 n2)) >>= pure.increasePC
    (0x4, x, n1, n2) -> Just $ (skipInstructionIf (\cpu -> CPU.getRegister x cpu /= Bits.twoWords4toWord8 n1 n2)) >>= pure.increasePC
    (0x5, x, y, 0x0) -> Just $ (skipInstructionIf (\cpu -> CPU.getRegister x cpu == CPU.getRegister y cpu)) >>= pure.increasePC
    (0x6, x, n1, n2) -> Just $ (setRegister x $ Bits.twoWords4toWord8 n1 n2) >>= pure.increasePC
    (0x7, x, n1, n2) -> Just $ (addRegister x $ Bits.twoWords4toWord8 n1 n2) >>= pure.increasePC
    (0x8, x, y, 0x0) -> Just $ (moveRegister x y) >>= pure.increasePC
    (0x8, x, y, 0x1) -> Just $ (orRegisters x y) >>= pure.increasePC
    (0x8, x, y, 0x2) -> Just $ (andRegisters x y) >>= pure.increasePC
    (0x8, x, y, 0x3) -> Just $ (xorRegisters x y) >>= pure.increasePC
    (0x8, x, y, 0x4) -> Just $ (addRegisters x y) >>= pure.increasePC
    (0x8, x, y, 0x5) -> Just $ (subtractRegisters x x y) >>= pure.increasePC
    (0x8, x, y, 0x7) -> Just $ (subtractRegisters x y x) >>= pure.increasePC
    (0x9, x, y, 0x0) -> Just $ (skipInstructionIf (\cpu -> CPU.getRegister x cpu /= CPU.getRegister y cpu)) >>= pure.increasePC
    (0xa, n1, n2, n3) -> Just $ (setIndex $ Bits.threeWords4toWord16 n1 n2 n3) >>= pure.increasePC
    (0xb, n1, n2, n3) -> Just $ (jumpToAddress $ Bits.threeWords4toWord16 n1 n2 n3)    
    (0xc, reg, n1, n2) -> Just $ (setRegWithRand reg $ Bits.twoWords4toWord8 n1 n2) >>= pure.increasePC
    (0xd, reg1, reg2, times) -> Just $ (drawSprite reg1 reg2 (fromIntegral times)) >>= pure.increasePC
    (0xe, reg, 0xa, 0x1) -> Just $ (skipInstructionIf (\cpu -> not $ Lens.view CPU.keys cpu V.! (fromIntegral reg))) >>= pure.increasePC
    (0xf, reg, 0x0, 0x7) -> Just $ (setRegisterToDelayTimer reg) >>= pure.increasePC
    (0xf, reg, 0x1, 0x5) -> Just $ (setDelayTimer reg) >>= pure.increasePC
    (0xf, reg, 0x2, 0x9) -> Just $ (setSpriteIndex reg) >>= pure.increasePC
    (0xf, reg, 0x3, 0x3) -> Just $ (loadBcd reg) >>= pure.increasePC
    (0xf, length, 0x6, 0x5) -> Just $ (fillRegisters length) >>= pure.increasePC
    --(6, x, n1, n2) -> error (show (Bits.twoWords4toWord8 n1 n2))
    _ -> Nothing

moveRegister :: W.Word8 -> W.Word8 -> Instruction
moveRegister x y cpu = setRegister x (CPU.getRegister y cpu) cpu

andRegisters :: W.Word8 -> W.Word8 -> Instruction
andRegisters x y cpu = cpu & setRegister x ((CPU.getRegister x cpu) .&. (CPU.getRegister y cpu))

orRegisters :: W.Word8 -> W.Word8 -> Instruction
orRegisters x y cpu = cpu & setRegister x ((CPU.getRegister x cpu) .|. (CPU.getRegister y cpu))

xorRegisters :: W.Word8 -> W.Word8 -> Instruction
xorRegisters x y cpu = cpu & setRegister x ((CPU.getRegister x cpu) `xor` (CPU.getRegister y cpu))

addRegisters :: W.Word8 -> W.Word8 -> Instruction
addRegisters x y cpu = cpu & Lens.over CPU.vx (V.// [(fromIntegral x, vx + vy), (0xf, carry)])
                       where vx = CPU.getRegister x cpu
                             vy = CPU.getRegister y cpu
                             carry = if (0xff :: W.Word16) < fromIntegral vx + fromIntegral vy then 1 else 0

subtractRegisters :: W.Word8 -> W.Word8 -> W.Word8 -> Instruction
subtractRegisters reg x y cpu = cpu & Lens.over CPU.vx (V.// [(fromIntegral reg, vx - vy), (0xf, carry)])
                       where vx = CPU.getRegister x cpu
                             vy = CPU.getRegister y cpu
                             carry = if fromIntegral vx < fromIntegral vy then 0 else 1

skipInstructionIf :: (CPU -> Bool) -> Instruction
skipInstructionIf condition cpu = if condition cpu then increasePC cpu else cpu 

setRegisterToDelayTimer :: W.Word8 -> Instruction
setRegisterToDelayTimer reg cpu = cpu & Lens.over CPU.vx (V.// [(fromIntegral reg, Lens.view CPU.delayTimer cpu)])

setDelayTimer :: W.Word8 -> Instruction
setDelayTimer reg cpu = cpu & Lens.set CPU.delayTimer (CPU.getRegister reg cpu)

setSpriteIndex :: W.Word8 -> Instruction
setSpriteIndex register cpu = cpu & Lens.over CPU.vx (V.// [(fromIntegral register, 5 * CPU.getRegister register cpu)])

fillRegisters :: W.Word8 -> Instruction
fillRegisters length cpu = let indices = V.enumFromN 0 (fromIntegral length)
                               memVals = V.slice (fromIntegral $ Lens.view CPU.index cpu) (fromIntegral length + 1) (Lens.view CPU.memory cpu)
                            in cpu & Lens.over CPU.vx (`V.update` (V.zip indices memVals))

loadBcd :: W.Word8 -> Instruction
loadBcd reg cpu = cpu &
                  Lens.over CPU.memory (V.// [(fromIntegral (Lens.view CPU.index cpu), (CPU.getRegister reg cpu) `div` 100)]) &
                  Lens.over CPU.memory (V.// [(fromIntegral ((Lens.view CPU.index cpu) + 1), ((CPU.getRegister reg cpu) `div` 10) `mod` 10)]) &
                  Lens.over CPU.memory (V.// [(fromIntegral ((Lens.view CPU.index cpu) + 2), (CPU.getRegister reg cpu) `mod` 10)])


jumpToAddress :: W.Word16 -> Instruction
jumpToAddress address = Lens.set CPU.pc address

jumpPlusIndex :: W.Word16 -> Instruction
jumpPlusIndex address cpu = Lens.set CPU.pc (address + (fromIntegral $ CPU.getRegister 0 cpu)) cpu

callSubroutine :: W.Word16 -> Instruction
callSubroutine address cpu = cpu & (CPU.pushToStack (Lens.view CPU.pc cpu)) & (jumpToAddress address)

returnFromSubroutine :: Instruction
returnFromSubroutine cpu = (snd popResult) & (Lens.set CPU.pc $ (fst popResult))
                           where popResult = CPU.popFromStack cpu

returnFromSubroutine cpu = snd $ CPU.popFromStack cpu

setRegister :: W.Word8 -> W.Word8 -> Instruction
setRegister x val = Lens.over CPU.vx (V.// [(fromIntegral x, val)])

addRegister :: W.Word8 -> W.Word8 -> Instruction
addRegister x val cpu = cpu & Lens.over CPU.vx (V.// [(fromIntegral x, val + CPU.getRegister x cpu)])

setIndex :: W.Word16 -> Instruction
setIndex val = Lens.set CPU.index val

-- |
-- Draws a sprite of size (8 * times) from memory at location (x,y)
-- Changes gfx (and register 0xF if collision detected)
drawSprite :: W.Word8 -> W.Word8 -> W.Word8 -> Instruction
drawSprite x y times cpu =
  let
    index  = Lens.view CPU.index cpu
    (collision, sprite) = arrangePixels (fromIntegral $ CPU.getRegister x cpu) (fromIntegral $ CPU.getRegister y cpu) (fromIntegral index) (fromIntegral times) cpu
  in
      Lens.over CPU.vx (V.// [(0xF, if collision then 1 else 0)]) $
        Lens.over CPU.graphics (`V.update` sprite) cpu

arrangePixels :: Int -> Int -> Int -> Int -> CPU.CPU -> (Bool, V.Vector (Int, Bool))
arrangePixels x y index times cpu =
  unmergePixels $
    V.zipWith3 mergePixels
      (indicesVector x y times)
      (V.backpermute
        (Lens.view CPU.graphics cpu)
        (indicesVector x y times))
      (V.concatMap byteVector $
        V.slice index times (Lens.view CPU.memory cpu))

unmergePixels :: V.Vector (Int, Bool, Bool) -> (Bool, V.Vector (Int, Bool))
unmergePixels vec =
  (V.foldl (\acc (_, o, n) -> acc || (o && not (o `xor` n))) False vec
  ,V.map (\(i, o, n) -> (i, o `xor` n)) vec
  )

mergePixels :: Int -> Bool -> Bool -> (Int, Bool, Bool)
mergePixels = (,,)


indicesVector :: Int -> Int -> Int -> V.Vector Int
indicesVector x y times =

  V.concatMap
    (V.map (`mod` 2048) . separateIndex)
    (V.enumFromStepN (y*64 + x) 64 times)

separateIndex :: Int -> V.Vector Int
separateIndex i =
  V.fromList [i..i+7]

byteVector :: W.Word8 -> V.Vector Bool
byteVector byte = V.fromList (reverse $ go 0x1)
  where go 0x80 = [0x80 == 0x80 .&. byte]
        go n    = (n    == n    .&. byte) : go (n `shiftL` 1)

setRegWithRand :: W.Word8 -> W.Word8 -> Instruction
setRegWithRand reg nn cpu =
  setRegister reg (randNum .&. nn) $ Lens.set CPU.randomSeed nextSeed cpu
  where (randNum, nextSeed) = Rand.random $ Lens.view CPU.randomSeed cpu

