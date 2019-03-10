{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module CPU.CPU where

-------------
-- Imports
-------------

import qualified Data.Word as W (Word8, Word16)
import qualified Data.Vector as V
import qualified Lens.Micro as Lens (set, over)
import Lens.Micro ((&))
import qualified Lens.Micro.Mtl as Lens (view)
import Lens.Micro.TH (makeLenses)
import qualified System.Random as Rand
import qualified CPU.Bits as Bits

---------------
-- data & type
---------------

type DataRegister = W.Word8
type AddressRegister = W.Word16
data Stack a = Empty | Cons a (Stack a)

data CPU = CPU {
    _memory :: V.Vector DataRegister, -- 4096 bytes
    _pc :: W.Word16,
    _vx :: V.Vector DataRegister,
    _stack :: Stack W.Word16,
    _delayTimer :: DataRegister,
    _soundTimer :: DataRegister,
    _index :: AddressRegister,
    _graphics :: V.Vector Bool, -- 64x32 pixels,
    _keys :: V.Vector Bool,  -- 16 keys
    _randomSeed :: Rand.StdGen
}

instance Show CPU where
  show cpu = unlines $ map ($ cpu)
    [Bits.showHex16 . _pc
    ,show . _index
    ,show . _delayTimer
    ,show . _soundTimer
    ,show . _vx
    ,show . _keys
    ,show . _memory
    ,show . _graphics]

makeLenses ''CPU

---------------
-- CPU utilities
---------------

-- inits CPU with defualt vlaues
initCPU :: CPU
initCPU = CPU {
    _memory = V.replicate 4096 0,
    _pc = 0x200,
    _vx = V.replicate 16 0,
    _stack = initStack 12,
    _delayTimer = 0,
    _soundTimer = 0,
    _index = 0,
    _graphics = V.replicate (64 * 32) False,
    _keys = V.replicate 16 False,
    _randomSeed = Rand.mkStdGen 100
}

push:: Stack a -> Stack a -> Stack a
push Empty x = x
push (Cons x  xs) ys = Cons x (push xs ys)

instance Functor Stack where
    fmap f Empty = Empty
    fmap f (Cons w1 w2) = Cons (f w1) (fmap f w2)

instance Applicative Stack where
    pure x = Cons x Empty
    Empty <*> _ = Empty
    (Cons f fs) <*> xs = fmap f xs `push` (fs <*> xs)

instance Monad Stack where
    Empty >>= _ = Empty
    (Cons x xs) >>= f = (f x) `push` (xs >>= f) 

getPC :: CPU -> Int
getPC = fromIntegral . Lens.view pc

-- inits CPU's stack with 12 0's
initStack:: Int -> Stack W.Word16
initStack 1 = Cons 0 Empty 
initStack n = Cons 0 (initStack $ n-1)

--push value to stack and return the new stack
pushToStack :: W.Word16 -> CPU -> CPU
pushToStack x cpu = cpu & Lens.set stack ((Cons x (Lens.view stack cpu)) >>= pure)

--pop from stack and return the new stack and the top as tuple
popFromStack :: CPU -> (W.Word16, CPU)
popFromStack cpu = (getValStack (Lens.view stack cpu), cpu & Lens.set stack (getRestStack  $ Lens.view stack cpu))

--return the stack without the head
getRestStack:: Stack a -> Stack a
getRestStack (Cons x xs) = xs

--return the top of the stack
getValStack:: Stack a -> a
getValStack (Cons x xs) = x

--set all keys to false (unpressed)
clearKeys :: CPU -> CPU
clearKeys cpu = cpu & Lens.set keys (V.replicate 16 False)

--return the register in the given index
getRegister :: W.Word8 -> CPU -> DataRegister
getRegister index cpu = Lens.view vx cpu V.! (fromIntegral index)

type Error = String

showErr :: Error -> String
showErr err = "Error: " ++ err
