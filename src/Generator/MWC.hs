module Generator.MWC (mwc32, mwc64) where

import Control.Monad (replicateM)
import Control.Monad.ST (runST)
import Data.Word (Word32, Word64)
import Data.Vector.Unboxed (singleton)
import System.Random.MWC (initialize, uniform)

mwc32 :: Word32 -> Int -> [Word32]
mwc32 seed num
  | num < 1 = []
  | otherwise = runST $ initialize (singleton seed) >>= \gen -> replicateM num $ uniform gen

mwc64 :: Word32 -> Int -> [Word64]
mwc64 seed num
  | num < 1 = []
  | otherwise = runST $ initialize (singleton seed) >>= \gen -> replicateM num $ uniform gen
