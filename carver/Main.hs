import Image.Processing.SeamCarving
import Codec.Image.DevIL

main = do
  ilInit
  image <- readImage "/home/mma/1.png"
  let newImage = removeHorizontals 50 image
  writeImage "/home/mma/output.png" newImage
  return ()
