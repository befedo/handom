module Generator.PCG (pcg32, pcg64) where

import Control.Monad (replicateM)
import Control.Monad.ST (runST)
import Data.Word (Word32, Word64)
import System.Random.PCG (initialize, uniformW32, uniformW64)

pcg32 :: Word64 -> Word64 -> Int -> [Word32]
pcg32 a b num
  | num < 1 = []
  | otherwise = runST $ initialize a b >>= \gen -> replicateM num $ uniformW32 gen

pcg64 :: Word64 -> Word64 -> Int -> [Word64]
pcg64 a b num
  | num < 1 = []
  | otherwise = runST $ initialize a b >>= \gen -> replicateM num $ uniformW64 gen
