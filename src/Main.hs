{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
import Data.Word (Word32, Word64)
import Data.Int  (Int64)
import Generator.Isaac (isaac)
import Generator.Lehmer (lehmer)
import Generator.MWC (mwc32, mwc64)
import Generator.PCG (pcg32, pcg64)
import Generator.SplitMix (splitMix32, splitMix64)
import Generator.Xorshift128Plus (xorshift128Plus)
import Text.Printf (printf)

import Data.Binary.Put (putWord32le, putWord64le, runPut)
import qualified Data.ByteString.Lazy as BsLazy (writeFile)

import System.Console.CmdArgs ((&=), Data, Typeable, cmdArgs, def, explicit, help, helpArg, modes, name, program, typ, versionArg)
import System.Environment (getArgs, withArgs)

data Opts = Isaac {chars :: String, length_ :: Int, file :: String}
          | Lehmer {seed :: Int, length_ :: Int, file :: String}
          | Mwc {word32 :: Word32, length_ :: Int, file :: String, wide :: Bool}
          | Pcg {b_seed :: Word64, a_seed :: Word64, length_ :: Int, file :: String, wide :: Bool}
          | SplitMix {int :: Int64, length_ :: Int, file :: String, wide :: Bool}
          | Xoroshiro {word64 :: Word64, length_ :: Int, file :: String}
          deriving (Show, Data, Typeable)

isaacOpts :: Opts
isaacOpts = Isaac
  { chars = def &= help "Seed for the ISAAC-Generator" &= typ "String" &= name "seed" &= name "s" &= explicit
  , length_ = def &= help "Output length (number of values)" &= typ "Int"
  , file = def &= help "Specify the binary output File" &= typ "String"
  }

lehmerOpts :: Opts
lehmerOpts = Lehmer
  { seed = def &= help "Seed for the Lehmer-Generator" &= typ "Int" &= name "seed" &= name "s" &= explicit
  , length_ = def &= help "Output length (number of values)" &= typ "Int"
  , file = def &= help "Specify the binary output File" &= typ "String"
  }

mwcOpts :: Opts
mwcOpts = Mwc
  { word32 = def &= help "Seed for the MWC-Generator" &= typ "Word32" &= name "seed" &= name "s" &= explicit
  , length_ = def &= help "Output length (number of values)" &= typ "Int"
  , wide = def &= help "Output width (wide defines 64Bit output while the default is 32Bit)" &= typ "Bool"
  , file = def &= help "Specify the binary output File" &= typ "String"
  }
pcgOpts :: Opts
pcgOpts = Pcg
  { b_seed = def &= help "Lower Part of the Seed for the PCG-Generator" &= typ "Word64"
  , a_seed = def &= help "Upper Part of the Seed for the PCG-Generator" &= typ "Word64"
  , length_ = def &= help "Output length (number of values)" &= typ "Int"
  , wide = def &= help "Output width (wide defines 64Bit output while the default is 32Bit)" &= typ "Bool"
  , file = def &= help "Specify the binary output File" &= typ "String"
  }

splitMixOpts :: Opts
splitMixOpts = SplitMix
  { int = def &= help "Seed for the SplitMix-Generator" &= typ "Int64" &= name "seed" &= name "s" &= explicit
  , length_ = def &= help "Output length (number of values)" &= typ "Int"
  , wide = def &= help "Output width (wide defines 64Bit output while the default is 32Bit)" &= typ "Bool"
  , file = def &= help "Specify the binary output File" &= typ "String"
  }

xoroshiroOpts :: Opts
xoroshiroOpts = Xoroshiro
  { word64 = def &= help "Seed for the Xoroshiro128Plus-Generator" &= typ "Word64" &= name "seed" &= name "s" &= explicit
  , length_ = def &= help "Output length (number of values)" &= typ "Int"
  , file = def &= help "Specify the binary output File" &= typ "String"
  }

main :: IO ()
main = do
  args <- getArgs
  opts <- (if null args then withArgs ["--help"] else id) getOpts
  handleOpts opts

getOpts = cmdArgs $ modes [isaacOpts, lehmerOpts, mwcOpts, pcgOpts, splitMixOpts, xoroshiroOpts]
  &= program "handom"
  &= help "A command line program for generating Pseudo-Random-Numbers."
  &= helpArg [explicit, name "help", name "h"]

handleOpts :: Opts -> IO ()
handleOpts o@Mwc{}       = handleMwc (wide o) (word32 o) (length_ o) (file o)
handleOpts o@Pcg{}       = handlePcg (wide o) (a_seed o) (b_seed o) (length_ o) (file o)
handleOpts o@Isaac{}     = handleIsaac (chars o) (length_ o) (file o)
handleOpts o@Lehmer{}    = handleLehmer (seed o) (length_ o) (file o)
handleOpts o@SplitMix{}  = handleSplitMix (wide o) (int o) (length_ o) (file o)
handleOpts o@Xoroshiro{} = handleXoroshiro (word64 o) (length_ o) (file o)

handleMwc :: Bool -> Word32 -> Int -> String -> IO ()
handleMwc wide seed length_ file
  | wide     && file == "" = mapM_ (printf "0x%016X\n") (mwc64 seed length_)
  | wide     && file /= "" = writeBin64 (mwc64 seed length_) file
  | not wide && file == "" = mapM_ (printf "0x%08X\n") (mwc32 seed length_)
  | otherwise              = writeBin32 (mwc32 seed length_) file

handlePcg :: Bool -> Word64 -> Word64 -> Int -> String -> IO ()
handlePcg wide a b length_ file
  | wide     && file == "" = mapM_ (printf "0x%016X\n") (pcg64 a b length_)
  | wide     && file /= "" = writeBin64 (pcg64 a b length_) file
  | not wide && file == "" = mapM_ (printf "0x%08X\n") (pcg32 a b length_)
  | otherwise              = writeBin32 (pcg32 a b length_) file

handleIsaac :: String -> Int -> String -> IO ()
handleIsaac chars length_ file
  | file == "" = mapM_ (printf "0x%08X\n") (isaac chars length_)
  | otherwise  = writeBin32 (isaac chars length_) file

handleLehmer :: Int -> Int -> String -> IO ()
handleLehmer seed length_ file
  | file == "" = mapM_ (printf "0x%08X\n") (lehmer seed length_)
  | otherwise  = writeBin32 (lehmer seed length_) file

handleSplitMix :: Bool -> Int64 -> Int -> String -> IO ()
handleSplitMix wide seed length_ file
  | wide     && file == "" = mapM_ (printf "0x%016X\n") (splitMix64 seed length_)
  | wide     && file /= "" = writeBin64 (splitMix64 seed length_) file
  | not wide && file == "" = mapM_ (printf "0x%08X\n") (splitMix32 seed length_)
  | otherwise              = writeBin32 (splitMix32 seed length_) file

handleXoroshiro :: Word64 -> Int -> String -> IO ()
handleXoroshiro seed length_ file
  | file == "" = mapM_ (printf "0x%016X\n") (xorshift128Plus seed length_)
  | otherwise  = writeBin64 (xorshift128Plus seed length_) file

writeBin32 :: [Word32] -> String -> IO ()
writeBin32 values file = BsLazy.writeFile file output
  where
  output = runPut $ mapM_ putWord32le values

writeBin64 :: [Word64] -> String -> IO ()
writeBin64 values file = BsLazy.writeFile file output
  where
  output = runPut $ mapM_ putWord64le values
