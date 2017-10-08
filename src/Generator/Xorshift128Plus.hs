{-# LANGUAGE MagicHash, UnboxedTuples #-}

-- This is an implementation of the xorshift128+ random number generator. Reading of
-- <http://xorshift.di.unimi.it/xorshift128plus.c> is recommended.
--
-- The generator state is stored in the 'Gen' data type which is created by the 'initialize'
-- function with a initial seed value or from directly calling the 'Gen' data constructor
-- with generator state.

module Generator.Xorshift128Plus (xorshift128Plus) where

import Data.Word(Word64(..))
import Data.Bits

-- Random number generator.
data Gen = Gen {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  deriving (Eq, Show)

-- Create a generator by seed value.
-- NOTE: do not supply 0
initialize :: Word64 -> Gen
initialize s = let s0 = 1812433253 * (s `xor` (s `shiftR` 30)) + 1
                   s1 = 1812433253 * (s0 `xor` (s0 `shiftR` 30)) + 2
               in  Gen s0 s1
{-# INLINE initialize #-}

-- Generate a 64bit random value and next generator state.
next :: Gen -> (Word64, Gen)
next g = let (# n, g' #) = next# g
         in  (n, g')
{-# INLINE next #-}

-- Same as 'next', but return values with unboxed tuple.
next# :: Gen -> (# Word64, Gen #)
next# g = let g'@(Gen s0 s1) = step g
          in  (# s0 + s1, g' #)
{-# INLINE next# #-}

step :: Gen -> Gen
step (Gen s0 s1) =
  let s1' = s0 `xor` (s0 `shiftL` 23)
  in  Gen s1 (s1' `xor` s1 `xor` (s1' `shiftR` 17) `xor` (s1 `shiftR` 26))
{-# INLINE step #-}

word64max :: Double
word64max = fromIntegral (maxBound :: Word64)
{-# INLINE word64max #-}

xorshift128Plus :: String -> Int -> [Word64]
xorshift128Plus input num = if num < 1 then stream gen' else take num (stream gen')
  where gen' = initialize (read input :: Word64)
        stream :: Gen -> [Word64]
        stream gen = value : stream seed
          where (value, seed) = next gen
