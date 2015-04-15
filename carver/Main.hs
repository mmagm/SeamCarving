import Image.Processing.SeamCarving
import Codec.Image.DevIL
import Data.Array.Unboxed

main = do
  ilInit
  image <- readImage "/home/mma/1.png"
  let newImage = removeVerticals 30 $ removeHorizontals 50 image
  putStrLn $ show $ bounds newImage
  writeImage "/home/mma/output.png" newImage
  return ()
