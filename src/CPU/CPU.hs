{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module CPU.CPU where

import qualified Data.Word as W (Word8, Word16)
import qualified Data.Vector as V
import qualified Lens.Micro as Lens (set, over)
import Lens.Micro ((&))
import qualified Lens.Micro.Mtl as Lens (view)
import Lens.Micro.TH (makeLenses)
import qualified System.Random as Rand

type Stack = V.Vector W.Word8
type DataRegister = W.Word8
type AddressRegister = W.Word16

data CPU = CPU {
    _memory :: V.Vector DataRegister, -- 4096 bytes
    _pc :: AddressRegister,
    _vx :: V.Vector DataRegister,
    _stack :: Stack,
    _sp :: DataRegister,
    _delayTimer :: DataRegister,
    _soundTimer :: DataRegister,
    _index :: AddressRegister,
    _graphics :: V.Vector Bool, -- 64x32 pixels,
    _keys :: V.Vector Bool,  -- 16 keys
    _randomSeed :: Rand.StdGen
}

makeLenses ''CPU

initCPU :: CPU
initCPU = CPU {
    _memory = V.replicate 4096 0,
    _pc = 0x200,
    _vx = V.replicate 16 0,
    _stack = V.replicate 12 0,
    _sp = 0,
    _delayTimer = 0,
    _soundTimer = 0,
    _index = 0,
    _graphics = V.replicate (64 * 32) False,
    _keys = V.replicate 16 False,
    _randomSeed = Rand.mkStdGen 100
}



getPC :: CPU -> Int
getPC = fromIntegral . Lens.view pc

getSP :: CPU -> Int
getSP = fromIntegral . Lens.view sp

pushToStack :: CPU -> W.Word8 -> CPU
pushToStack cpu x = cpu & Lens.over stack (V.// [(getSP cpu, x)])
    & Lens.over sp (+1)

popFromStack :: CPU -> (W.Word8, CPU)
popFromStack cpu = (Lens.view stack cpu V.! getSP cpu, Lens.over sp (\x -> x - 1) cpu)

clearKeys :: CPU -> CPU
clearKeys cpu = cpu & Lens.set keys (V.replicate 16 False)

type Error = String

showErr :: Error -> String
showErr err = "Error: " ++ err
