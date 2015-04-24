import Image.Processing.SeamCarving
import Codec.Image.DevIL
import Data.Array.Unboxed
import Options

data MainOptions = MainOptions
    {
      optInput :: FilePath
    , optOutput :: FilePath
    , optWidth :: Int
    , optHeight :: Int
    }

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "input" "/path/to/infile"
            "input file path"
        <*> simpleOption "output" "/path/to/outfile"
            "output file path"
        <*> simpleOption "width" 20
            "Shrink 20 pixels on width."
        <*> simpleOption "height" 30
            "Shrink 20 pixels on height."

main :: IO ()
main = runCommand $ \opts args -> do
  ilInit
  image <- readImage $ optInput opts
  let widthShrink = optWidth opts
      heightShrink = optHeight opts
  let newImage = removeVerticals widthShrink $ removeHorizontals heightShrink image
  putStrLn $ show $ bounds newImage
  writeImage (optOutput opts) newImage
  return ()
