-- This is a port of "Fast Splittable Pseudorandom Number Generators" by Steele et. al. [1].
--
-- The paper's algorithm provides decent randomness for most purposes but sacrifices
-- cryptographic-quality randomness in favor of speed. The original implementation is tested
-- with DieHarder and BigCrush; see the paper for details.
--
-- 1. Guy L. Steele, Jr., Doug Lea, Christine H. Flood
--    Fast splittable pseudorandom number generators
--    Comm ACM, 49(10), Oct 2014, pp453-472.

module Generator.SplitMix (splitMix32, splitMix64) where

import Data.Bits (popCount, shiftR, xor, (.|.))
import Data.Int  (Int32, Int64)
import Data.Word (Word32, Word64)

data Seed = Seed
  { value :: Int64
  , gamma :: Int64
  }

-- A predefined gamma value's needed for initializing the "root" instances of SplittableRandom
-- that is, instances not produced by splitting an already existing instance. We choose:
--   the odd integer closest to 2^64/φ, where φ = (1 + √5)/2 is the golden ratio,
--   and call it GOLDEN_GAMMA.
goldenGamma :: Int64
goldenGamma =  -7046029254386353131

-- Mix the bits of a 64-bit arg to produce a result, with a bijective function on 64-bit values.
mix64 :: Int64 -> Int64
mix64 x =
  let y = (x `xor` (x `shiftR` 33)) * (-49064778989728563)
      z = (y `xor` (y `shiftR` 33)) * (-4265267296055464877)
   in z `xor` (z `shiftR` 33)

-- Mix the bits of a 64-bit arg to produce a result, with a bijective function on 64-bit values.
mix32 :: Int64 -> Int32
mix32 x =
  let y = (x `xor` (x `shiftR` 33)) * (-49064778989728563)
      z = (y `xor` (y `shiftR` 33)) * (-4265267296055464877)
   in fromIntegral (z `shiftR` 32)

-- Mix the bits of a 64-bit arg to produce a result, with a bijective function on 64-bit values.
mix64variant13 :: Int64 -> Int64
mix64variant13 x =
  let y = (x `xor` (x `shiftR` 30)) * (-4658895280553007687)
      z = (y `xor` (y `shiftR` 27)) * (-7723592293110705685)
   in z `xor` (z `shiftR` 31)

-- Mix the bits of a 64-bit arg to produce a result, with a bijective function on 64-bit values.
mixGamma :: Int64 -> Int64
mixGamma x =
  let y = mix64variant13 x .|. 1
      n = popCount $ y `xor` (y `shiftR` 1)
   in if n < 24 then y `xor` (-6148914691236517206) else y

ofInt64 :: Int64 -> Seed
ofInt64 x =
  let rnd = x + (2 * goldenGamma) in
  Seed
    { value = mix64    rnd,
      gamma = mixGamma rnd + goldenGamma }

nextSeed :: Seed -> Seed
nextSeed s0 =
  s0 { value = value s0 + gamma s0 }

split :: Seed -> (Seed, Seed)
split s0 =
  let s1 = nextSeed s0
      s2 = nextSeed s1
  in (s0 { value = mix64 (value s1) }
    , s1 { value = mix64 (value s2) })

nextInt32 :: Seed -> Int32
nextInt32 s0 = let s1 = nextSeed s0 in mix32 (value s1)

nextInt64 :: Seed -> Int64
nextInt64 s0 = let s1 = nextSeed s0 in mix64 (value s1)

splitMix32 :: Int64 -> Int -> [Word32]
splitMix32 seed num
  | num < 1   = []
  | otherwise = take num $ stream (ofInt64 seed)
  where
  stream :: Seed -> [Word32]
  stream seed = (fromIntegral (nextInt32 seed) :: Word32) : stream (nextSeed seed)

splitMix64 :: Int64 -> Int -> [Word64]
splitMix64 seed num
  | num < 1   = []
  | otherwise = take num $ stream (ofInt64 seed)
  where
  stream :: Seed -> [Word64]
  stream seed = (fromIntegral (nextInt64 seed) :: Word64) : stream (nextSeed seed)
