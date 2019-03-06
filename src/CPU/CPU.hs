{-# LANGUAGE LambdaCase #-}

module CPU.Emulate where

-------------
-- Imports
-------------

import Control.Monad ((>=>))
import qualified Data.Word as W (Word8, Word16)
import qualified Data.Vector as V
import Data.Bits ((.&.), (.|.), xor, shiftR, shiftL)
import Data.Bits
import Lens.Micro ((&))
import qualified Lens.Micro     as Lens
import qualified Lens.Micro.Mtl as Lens
import qualified Data.ByteString as BS
import qualified System.Random as Rand

import CPU.CPU (CPU, Error)
import qualified CPU.CPU as CPU
import qualified CPU.Bits as Bits

-------------
-- Loading
-------------

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

loadGameAndFonts :: BS.ByteString -> Emulate CPU
loadGameAndFonts =
  loadGame >=> pure . loadFonts

loadGame :: BS.ByteString -> Emulate CPU
loadGame buffer = Right $ cpu & Lens.over CPU.memory (\mem -> V.update_ mem (V.enumFromN 0x200 (length unpackedBuffer)) (V.fromList unpackedBuffer))
         where cpu = CPU.initCPU
               unpackedBuffer = BS.unpack buffer

loadFonts :: CPU -> CPU
loadFonts = Lens.over CPU.memory (\mem -> V.update_ mem (V.enumFromN 0 (V.length fontSet)) fontSet)

---------------
-- Emulation
---------------

type Emulate a = Either Error a
type Instruction = (CPU -> Emulate CPU)

emulateCycle :: CPU -> Emulate CPU
emulateCycle cpu = execute cpu $ decode cpu $ fetch cpu

merge16 :: W.Word8 -> W.Word8 -> W.Word16
merge16 high low = shift (fromIntegral high) 8 .|. fromIntegral low

fetch :: CPU -> W.Word16
fetch cpu =
  case cmd of
    Just instruction ->
      instruction

  where cmd = merge16 <$> (pure $ Lens.view CPU.memory cpu V.! CPU.getPC cpu)
                           <*> (pure $ Lens.view CPU.memory cpu V.! (CPU.getPC cpu + 1))

decode :: CPU -> W.Word16 -> Instruction
decode cpu cmd =
 case findOpcode cmd of
    Just instruction ->
      instruction


execute :: CPU -> Instruction -> Emulate CPU
execute cpu instruction = instruction cpu

updateTimers :: CPU -> CPU
updateTimers cpu =
  cpu & Lens.over CPU.delayTimer (\dt -> if dt > 0 then dt - 1 else 0)
      & Lens.over CPU.soundTimer (\st -> if st > 0 then st - 1 else 0)

cleanSoundTimer :: CPU -> CPU
cleanSoundTimer =
   Lens.set CPU.soundTimer 0

----------------
-- update CPU
----------------

-- |
-- increases the program counter by 2
nextPC :: CPU -> CPU
nextPC =
  Lens.over CPU.pc (+2)

-------------
-- Opcodes
-------------

-- |
-- finds the relevant instruction from opcode.
-- Based on this: https://en.wikipedia.org/wiki/CHIP-8#Opcode_table
findOpcode :: W.Word16 -> Maybe Instruction
findOpcode opcode =
  case Bits.splitOpcode opcode of
    (0x0, 0x0, 0xE, 0x0) ->
      pure $ pure . nextPC >=> clearScreen
    (0x0, 0x0, 0xE, 0xE) ->
      -- "Returns from a subroutine."
      pure $ pure . nextPC >=> returnFromSubroutine
    (0x1, n1, n2, n3) ->
      pure $ jumpToAddress $ Bits.threeWords4toWord16 n1 n2 n3
    (0x2, n1, n2, n3) ->
      pure $ pure . nextPC >=> callSubroutine (Bits.threeWords4toWord16 n1 n2 n3)
    (0x3, reg, n1, n2) ->
      pure $ pure . nextPC >=> skipInstructionIf (\cpu -> CPU.getRegister reg cpu == Bits.twoWords4toWord8 n1 n2)
    (0x4, reg, n1, n2) ->
      pure $ pure . nextPC >=> skipInstructionIf (\cpu -> CPU.getRegister reg cpu /= Bits.twoWords4toWord8 n1 n2)
    (0x5, x, y, 0x0) ->
      pure $ pure . nextPC >=> skipInstructionIf (\cpu -> CPU.getRegister x cpu == CPU.getRegister y cpu)
    (0x6, x, n1, n2) ->
      pure $ pure . nextPC >=> setRegister x (Bits.twoWords4toWord8 n1 n2)
    (0x7, x, n1, n2) ->
      pure $ pure . nextPC >=> addRegister x (Bits.twoWords4toWord8 n1 n2)
    (0x8, x, y, 0x0) ->
      pure $ pure . nextPC >=> movRegister x y
    (0x8, x, y, 0x1) ->
      pure $ pure . nextPC >=> orRegisters x y
    (0x8, x, y, 0x2) ->
      pure $ pure . nextPC >=> andRegisters x y
    (0x8, x, y, 0x3) ->
      pure $ pure . nextPC >=> xorRegisters x y
    (0x8, x, y, 0x4) ->
      pure $ pure . nextPC >=> addRegisters x y
    (0x8, x, y, 0x5) ->
      pure $ pure . nextPC >=> subtractRegisters x x y
    (0x8, x, y, 0x7) ->
      pure $ pure . nextPC >=> subtractRegisters y x y
    (0x9, x, y, 0x0) ->
      pure $ pure . nextPC >=> skipInstructionIf (\cpu -> CPU.getRegister x cpu /= CPU.getRegister y cpu)
    (0xA, n1, n2, n3) ->
      pure $ pure . nextPC >=> setIndex (Bits.threeWords4toWord16 n1 n2 n3)
    (0xB, n1, n2, n3) ->
      pure $ jumpPlusIndex $ Bits.threeWords4toWord16 n1 n2 n3
    (0xC, reg, n1, n2) ->
      -- "Sets VX to the result of a bitwise and operation on a random number and NN." -- not yet implemented
      pure $ pure . nextPC >=> setRegWithRand reg (Bits.twoWords4toWord8 n1 n2)
    (0xD, reg1, reg2, times) ->
      -- "Sprites stored in memory at location in index register (I), 8bits wide. Wraps around the screen. If when drawn, clears a pixel, register VF is set to 1 otherwise it is zero. All drawing is XOR drawing (i.e. it toggles the screen pixels). Sprites are drawn starting at position VX, VY. N is the number of 8bit rows that need to be drawn. If N is greater than 1, second line continues at position VX, VY+1, and so on." -- not yet implemented
      pure $ pure . nextPC >=> drawSprite reg1 reg2 (fromIntegral times)
    (0xE, reg, 0x9, 0xE) ->
      -- "Skips the next instruction if the key stored in VX is pressed."
      pure $ pure . nextPC >=> skipInstructionIf (\cpu -> Lens.view CPU.keys cpu V.! (fromIntegral (CPU.getRegister reg cpu)))
    (0xE, reg, 0xA, 0x1) ->
      -- "Skips the next instruction if the key stored in VX isn't pressed."
      pure $ pure . nextPC >=> skipInstructionIf (\cpu -> not $ Lens.view CPU.keys cpu V.! (fromIntegral (CPU.getRegister reg cpu)))
    (0xF, reg, 0x0, 0x7) ->
      pure $ pure . nextPC >=> \cpu -> pure $ cpu & Lens.over CPU.vx (V.// [(fromIntegral reg, Lens.view CPU.delayTimer cpu)])
    (0xF, reg, 0x1, 0x5) ->
      pure $ pure . nextPC >=> \cpu -> pure $ Lens.set CPU.delayTimer (CPU.getRegister reg cpu) cpu
    (0xF, reg, 0x1, 0x8) ->
      pure $ pure . nextPC >=> \cpu -> pure $ Lens.set CPU.soundTimer (CPU.getRegister reg cpu) cpu
    (0xF, reg, 0x1, 0xE) ->
      pure $ pure . nextPC >=> \cpu -> cpu & setIndex (fromIntegral $ (fromIntegral $ Lens.view CPU.index cpu) + (CPU.getRegister reg cpu))
    (0xF, reg, 0x2, 0x9) ->
      -- "Sets I to the location of the sprite for the character in VX. Characters 0-F (in hexadecimal) are represented by a 4x5 font."
      pure $ pure . nextPC >=> \cpu -> pure $ cpu & Lens.set CPU.index (5 * fromIntegral (CPU.getRegister reg cpu))
    (0xF, reg, 0x3, 0x3) ->
      pure $ pure . nextPC >=> loadBcd reg
    (0xF, reg, 0x6, 0x5) ->
      pure $ pure . nextPC >=> fillRegisters reg
    _ -> Nothing -- Unrecognized opcode

fillRegisters :: W.Word8 -> Instruction
fillRegisters length cpu = let indices = V.enumFromN 0 (fromIntegral length + 1)
                               memVals = V.slice (fromIntegral $ Lens.view CPU.index cpu) (fromIntegral length + 1) (Lens.view CPU.memory cpu)
                            in pure $ cpu & Lens.over CPU.vx (`V.update` (V.zip indices memVals))

loadBcd :: W.Word8 -> Instruction
loadBcd reg cpu = pure $ cpu &
                  Lens.over CPU.memory (V.// [(fromIntegral (Lens.view CPU.index cpu), (CPU.getRegister reg cpu) `div` 100)]) &
                  Lens.over CPU.memory (V.// [(fromIntegral ((Lens.view CPU.index cpu) + 1), ((CPU.getRegister reg cpu) `div` 10) `mod` 10)]) &
                  Lens.over CPU.memory (V.// [(fromIntegral ((Lens.view CPU.index cpu) + 2), (CPU.getRegister reg cpu) `mod` 10)])
-- |
-- Opcode 0x00E0
-- Clears the screen
clearScreen :: Instruction
clearScreen =
  pure . Lens.over CPU.graphics (V.map (const False))

-- |
-- Opcode 0x1nnn
-- Jump to address 0x0nnn
-- changes the program counter to given address
jumpToAddress :: W.Word16 -> Instruction
jumpToAddress address = pure .Lens.set CPU.pc address

-- |
-- Opcode 0x2nnn
-- Call a subroutine on address 0x0nnn
-- Saves the program counter on the stack and changes it to given address
callSubroutine :: W.Word16 -> Instruction
callSubroutine address cpu = cpu & (CPU.pushToStack (Lens.view CPU.pc cpu)) & (jumpToAddress address)

returnFromSubroutine :: Instruction
returnFromSubroutine cpu = pure $ (snd popResult) & (Lens.set CPU.pc $ (fst popResult))
                           where popResult = CPU.popFromStack cpu

returnFromSubroutine cpu = pure $ snd $ CPU.popFromStack cpu

-- |
-- Opcodes 0x3vnn, 0x4vnn, 0x5xy0, 0x9xy0
-- Skips instruction if (test cpu) is true
-- may change the program counter (pc)
skipInstructionIf :: (CPU -> Bool) -> Instruction
skipInstructionIf condition cpu = if condition cpu then pure $ nextPC cpu else pure cpu 


-- |
-- Opcode 0xBnnn
-- Jump to address 0x0nnn + content of index register
-- changes the program counter
jumpPlusIndex :: W.Word16 -> Instruction
jumpPlusIndex address cpu = pure $ Lens.set CPU.pc (address + (fromIntegral $ Lens.view CPU.index cpu)) cpu

-- |
-- Opcode 0xFx33
-- Write the binary-coded decimal of register x to memory
-- at addresses index, index+1 and index+2
-- Changes the memory
storeBinRep :: W.Word8 -> Instruction
storeBinRep reg cpu =
  pure $ cpu &
                  Lens.over CPU.memory (V.// [(fromIntegral (Lens.view CPU.index cpu), (CPU.getRegister reg cpu) `div` 100)]) &
                  Lens.over CPU.memory (V.// [(fromIntegral ((Lens.view CPU.index cpu) + 1), ((CPU.getRegister reg cpu) `div` 10) `mod` 10)]) &
                  Lens.over CPU.memory (V.// [(fromIntegral ((Lens.view CPU.index cpu) + 2), (CPU.getRegister reg cpu) `mod` 10)])


-- |
-- Opcode 0xAnnn
-- Sets the index register to the immediate value nnn
-- changes the index register
setIndex :: W.Word16 -> Instruction
setIndex address =
  pure . Lens.set CPU.index address


-- |
-- Opcode 0x6vnn
-- Sets the register v to the immediate value nn
-- changes the register v
setRegister :: W.Word8 -> W.Word8 -> Instruction
setRegister x val = pure . Lens.over CPU.vx (V.// [(fromIntegral x, val)])

-- |
-- Opcode 0x7vnn
-- Add the immediate value nn to the register v
-- changes the register v
addRegister :: W.Word8 -> W.Word8 -> Instruction
addRegister x val cpu = pure $ cpu & Lens.over CPU.vx (V.// [(fromIntegral x, val + CPU.getRegister x cpu)])


-- |
-- Opcode 0x8xy0
-- Sets the register x to the content of register y
-- changes the register x
movRegister :: W.Word8 -> W.Word8 -> Instruction
movRegister x y cpu = setRegister x (CPU.getRegister y cpu) cpu

-- |
-- Opcode 0x8xy1
-- Sets the register x to (x | y)
-- changes the register x
orRegisters :: W.Word8 -> W.Word8 -> Instruction
orRegisters x y cpu = cpu & setRegister x (((CPU.getRegister x cpu) .|. (CPU.getRegister y cpu)) .&. 0xff)


-- |
-- Opcode 0x8xy2
-- Sets the register x to (x & y)
-- changes the register x
andRegisters :: W.Word8 -> W.Word8 -> Instruction
andRegisters x y cpu = cpu & setRegister x ((CPU.getRegister x cpu) .&. (CPU.getRegister y cpu))

-- |
-- Opcode 0x8xy3
-- Sets the register x to (x `xor` y)
-- changes the register x
xorRegisters :: W.Word8 -> W.Word8 -> Instruction
xorRegisters x y cpu = cpu & setRegister x ((CPU.getRegister x cpu) `xor` (CPU.getRegister y cpu))



-- |
-- Opcode 0x8xy4
-- Adds the registers x and y and stores them at register x. sets carry in register F
-- changes the registers x and F
addRegisters :: W.Word8 -> W.Word8 -> Instruction
addRegisters x y cpu = pure $ cpu & Lens.over CPU.vx (V.// [(fromIntegral x, (vx + vy) .&. 0xff), (0xf, carry)])
                       where vx = CPU.getRegister x cpu
                             vy = CPU.getRegister y cpu
                             carry = if (0xff :: W.Word16) < fromIntegral vx + fromIntegral vy then 1 else 0

-- |
-- Opcode 0x8xy5
-- Subtract the registers y from x and store in x. sets borrow in register F
-- changes the registers x and F
subtractRegisters :: W.Word8 -> W.Word8 -> W.Word8 -> Instruction
subtractRegisters reg x y cpu = pure $ cpu & Lens.over CPU.vx (V.// [(fromIntegral reg, vx - vy), (0xf, carry)])
                       where vx = CPU.getRegister x cpu
                             vy = CPU.getRegister y cpu
                             carry = if fromIntegral vx < fromIntegral vy then 0 else 1

-- |
-- Opcode 0x8xy7
-- Subtract the registers x from y and store in x. sets borrow in register F
-- changes the registers x and F
subRegistersBackwards :: W.Word8 -> W.Word8 -> Instruction
subRegistersBackwards x y cpu =
  pure $ Lens.over CPU.vx (V.// [(fromIntegral x, vy - vx), (0xF, borrow)]) cpu
  where vx = CPU.getRegister x cpu
        vy = CPU.getRegister y cpu
        borrow = if vx > vy then 0 else 1


-- |
-- Draws a sprite of size (8 * times) from memory at location (x,y)
-- Changes gfx (and register 0xF if collision detected)
drawSprite :: W.Word8 -> W.Word8 -> W.Word8 -> Instruction
drawSprite x y times cpu =
  let
    index  = Lens.view CPU.index cpu
    (collision, sprite) = arrangePixels (fromIntegral $ CPU.getRegister x cpu) (fromIntegral $ CPU.getRegister y cpu) (fromIntegral index) (fromIntegral times) cpu
  in
      pure $ Lens.over CPU.vx (V.// [(0xF, if collision then 1 else 0)]) $
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

