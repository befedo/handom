module Generator.Lehmer (lehmer) where

import Data.Word (Word32)
import System.Random.Lehmer.Base (next)
import System.Random.Lehmer.State (LehmerState, lehmerInit)

lehmer :: Int -> Int -> [Word32]
lehmer seed num
  | num < 1 = []
  | otherwise = take num $ lehmer' (lehmerInit seed)
  where
  lehmer' :: LehmerState -> [Word32]
  lehmer' state = (fromIntegral value :: Word32) : lehmer' nextState
    where
    (value, nextState) = next state
