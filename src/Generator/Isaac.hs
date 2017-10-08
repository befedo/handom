-- This is a Haskell implementation of Robert Jenkins' stream cipher ISAAC.
-- ISAAC stands for Indirection, Shift, Accumulate, Add, and Count.
--
-- This implementation uses the full 2-pass key schedule that Jenkins suggests for use with ISAAC.
-- This implementation produces the same streams as Jenkin's C implementation.
--
-- Notice that this implementation will produce the same streams irregardless of the endianess of
-- the machine it is run upon, where Jenkins' C code will not.
--
-- Also notice that instead of generating the streams in blocks of 256 32-bit values, this
-- implementation uses Haskell's "lazy evaluation" in order to generate only the exact number of
-- values needed.

module Generator.Isaac (isaac) where

import Data.Array (Array, array, (//), (!))
import Data.Bits ((.|.), shift, shiftL, shiftR, xor)
import Data.Word (Word32)

type IsaacArray = Array Word32 Word32

intToWord32 :: Int -> Word32
intToWord32 int = fromIntegral int :: Word32

isaac :: String -> Int -> [Word32]
isaac c num = if num < 1 then stream else take num stream
  where stream      = genStream $ genkey (listToArray 0 c emptyarr)
        listToArray :: Int -> String -> IsaacArray -> IsaacArray
        listToArray 1024 (x:xs) arr = arr
        listToArray n    []     arr = arr
        listToArray n    (x:xs) arr = listToArray (n+1) xs newarr
          where newarr  = arr // [(arrelem, (arr!arrelem) .|. val)]
                arrelem = intToWord32 (n `div` 4)
                val     = intToWord32 (fromEnum x) `shiftL` ((n `mod` 4)*8)

genStream :: IsaacArray -> [Word32]
genStream arr = aux arr 0 1 1 0
  where aux :: IsaacArray -> Word32 -> Word32 -> Word32 -> Word32 -> [Word32]
        aux arr aa bb cc 256  = aux arr aa (bb+cc+1) (cc+1) 0
        aux arr aa bb cc iter = res : aux newarr newaa res cc (iter+1)
          where newaa  = (aa `xor` (aa `shift` qq)) + (arr ! ((iter + 128) `mod` 256))
                           where qq = case iter `mod` 4 of 0 -> 13;   1 -> -6;   2 -> 2;   3 -> -16
                x      = arr ! iter
                y      = newaa + bb + (arr ! ((x `shiftR` 2) `mod` 256))
                newarr = arr // [(iter, y)]
                res    = x + (newarr ! ((y `shiftR` 10) `mod` 256))

emptyarr :: IsaacArray
emptyarr = array (0,255) [(i,0) | i <- [0..255]]

genkey :: IsaacArray -> IsaacArray
genkey k = mixArray (mix $ mix $ mix $ mix (replicate 8 0x9e3779b9)) emptyarr k 0

mix :: [Word32] -> [Word32]
mix a = aux a 0
  where aux r 8 = r
        aux [a, b, c, d, e, f, g, h] x = aux [b+c,c,newa+d,e,f,g,h,newa] (x+1)
          where newa = a `xor` (b `shift` (-currx))
                currx = case x of 0 -> -11;  1 -> 2;  2 -> -8;  3 -> 16
                                  4 -> -10;  5 -> 4;  6 -> -8;  7 -> 9

pass :: [Word32] -> IsaacArray -> Int -> IsaacArray
pass tmix arr 256 = arr
pass tmix arr i   = pass newtm (arr // [(intToWord32 (off+i), newtm!!off) | off <- [0..7]]) (i+8)
                        where newtm = mix $ zipWith (+) tmix [arr!intToWord32 (i+off) | off <- [0..7]]

mixArray :: [Word32] -> IsaacArray -> IsaacArray -> Int -> IsaacArray
mixArray tmix arr k 256 = pass tmix arr 0
mixArray tmix arr k i   = mixArray newtm (arr // [(intToWord32 (i+off), newtm!!off) | off <- [0..7]]) k (i+8)
                            where newtm = mix $ zipWith (+) tmix [k!intToWord32 (i+off) | off <- [0..7]]
