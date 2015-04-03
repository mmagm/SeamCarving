import Control.Monad
import Control.Monad.ST
import Data.Ix
import Data.Array.ST
import Data.Array.Unboxed
import Data.List
import Codec.Image.DevIL

type Cell = (Float, (Int, Int))
type Seams = Array (Int, Int) Cell
type Seam = Array Int Int
type Img = Array (Int, Int) Float
type RGB = (Int, Int, Int)
type RGBImg = UArray (Int, Int, Int) Word8

infinity :: Float
infinity = 1.0/0.0

gradient :: (Num a) => RGB -> RGB -> a
gradient c1 c2 = let r = fromIntegral $ rgbRed c1 - rgbRed c2
                     g = fromIntegral $ rgbGreen c1 - rgbGreen c2
                     b = fromIntegral $ rgbBlue c1 - rgbBlue c2
                 in r * r + g * g + b * b
  where
    rgbRed (r, _, _) = r
    rgbGreen (_, g, _) = g
    rgbBlue (_, _, b) = b

toTuple :: [a] -> (a,a,a)
toTuple [a,b,c] = (a,b,c)

energy :: RGBImg -> Img
energy img = runSTArray $ do
  outcome <- newArray ((x, y), (m, n)) 0.0
  forM_ r $ \(i, j) -> do
    writeArray outcome (i, j) $ fill i j
  return outcome
  where
    ((x, y, _), (m, n, _)) = bounds img
    r = range ((x,y), (m, n))
    fill :: Int -> Int -> Float
    fill i j
       | (i == x || i == m || j == y || j == n) = 3.0 * 255.0 * 255.0
       | otherwise = let p1 = toTuple $ [fromIntegral $ img ! ((i - 1),j,  k) | k <- [0..2]]
                         p2 = toTuple $ [fromIntegral $ img ! ((i + 1),j,  k) | k <- [0..2]]
                         p3 = toTuple $ [fromIntegral $ img ! (i, (j - 1), k) | k <- [0..2]]
                         p4 = toTuple $ [fromIntegral $ img ! (i, (j + 1), k) | k <- [0..2]]
                     in (gradient p1 p2) + (gradient p3 p4)

distances :: Img -> Seams
distances e = runSTArray $ do
  arr <- newArray size (infinity, (-1,-1))

  forM_ (range ((x,y),(width,y))) $ \i -> do
    writeArray arr i (0.0, (-1,-1))

  forM_ (range ((x,y+1), (width, height))) $ \(col, row) -> do
    let to = (col, row)
        i1 = (col - 1, row - 1)
        i2 = (col,     row - 1)
        i3 = (col + 1, row - 1)

    when (inRange size i1) $ do
      relax arr i1 to

    relax arr i2 to

    when (inRange size i3) $ do
      relax arr i3 to

  return arr

  where
    size = bounds e
    ((x,y), (width, height)) = size

    relax arr from to = do
      ato <- readArray arr to
      afrom <- readArray arr from
      let dto = fst ato
          dfrom = fst afrom
          eto = e ! to
      when (dto > dfrom + eto) $ do
        writeArray arr to (dfrom + eto, from)
      return ()

findMin :: Seams -> (Int, Int)
findMin seams = snd $ minimumBy (\x y -> (fst x) `compare` (fst y)) elms
  where
    elms = map (\i -> seams ! (i, height)) [start..width]
    ((start,_), (width, height)) = bounds seams

minSeam :: (Int, Int) -> Seams -> Seam
minSeam from seams = runSTArray $ do
  seam <- newArray (start, end) (-1)
  findSeam from seam
  return seam
  where
    ((_,start), (_,end)) = bounds seams
    findSeam key seam
      | (key == (-1, -1)) = return ()
      | otherwise = do
          writeArray seam (snd key) (fst key)
          let newKey = snd $ seams ! key
          findSeam newKey seam

removeVerticalSeam :: Seam -> RGBImg -> RGBImg
removeVerticalSeam seam image = runSTUArray $ do
  newImage <- newArray (start, (w-1, h, c)) (0 :: Word8)

  forM_ (range (ys, h)) $ \row -> do
    let arow = (seam ! row)

    when (arow > -1) $ do
      forM_ (range ((xs, arow), (zs, c))) $ \(col, ch) -> do
        writeArray newImage (col, row, ch) (image ! (col, row, ch))

      forM_ (range ((arow, w - 1), (zs, c))) $ \(col, ch) -> do
        writeArray newImage (col, row, ch) (image ! (col + 1, row, ch))

  return newImage

  where
    (start, (w,h,c)) = bounds image
    (xs, ys, zs) = start

main = do
  ilInit
  image <- readImage "/home/mma/file.png"
  let en = energy image
  let seams = distances en
  let m = findMin seams
  let seam = minSeam m seams
  let newImage = removeVerticalSeam seam image
  writeImage "/home/mma/output.png" newImage
  return ()
