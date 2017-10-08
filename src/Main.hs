import Control.Applicative
import Data.Word (Word32)
import Generator.Isaac (isaac)
import Generator.SplitMix (splitMix)
import Generator.Xorshift128Plus (xorshift128Plus)
import Options
import Text.Printf (printf)

data MainOptions = MainOptions
  { optSeed   :: String
  , optLength :: Int
  , optTest   :: String
  }


instance Options MainOptions where
  defineOptions = pure MainOptions
    <*> defineOption optionType_string (\o -> o
          { optionShortFlags  = ['s']
          , optionLongFlags   = ["seed"]
          , optionDefault     = "3748374974327"
          , optionDescription = "The initial Seed of the Generator."
          })
    <*> defineOption optionType_int (\o -> o
          { optionShortFlags  = ['l']
          , optionLongFlags   = ["length"]
          , optionDefault     = 64
          , optionDescription = "The output length of the Generator."
          })
    <*> defineOption optionType_string (\o -> o
          { optionShortFlags  = ['t']
          , optionLongFlags   = ["type"]
          , optionDefault     = "isaac"
          , optionDescription = "The Type of the Generator."
          })


main :: IO ()
main = runCommand $ \opts args ->
  case optTest opts of
    "isaac"     -> mapM_ (printf "0x%X\n") (isaac (optSeed opts) (optLength opts))
    "splitmix"  -> mapM_ (printf "0x%X\n") (splitMix (optSeed opts) (optLength opts))
    "xoroshiro" -> mapM_ (printf "0x%X\n") (xorshift128Plus (optSeed opts) (optLength opts))
    _           -> print ("Unknown Generator Type: " ++ optTest opts)
