import Image.Processing.SeamCarving
import Codec.Image.DevIL
import Data.Array.Unboxed

main :: IO ()
main = do
  ilInit
  image <- readImage "/home/mma/1.png"
  let newImage = removeVerticals 300 $ removeHorizontals 500 image
  putStrLn $ show $ bounds newImage
  writeImage "/home/mma/output.png" newImage
  return ()
