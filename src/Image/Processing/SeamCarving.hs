{-# LANGUAGE BangPatterns #-}

module Image.Processing.SeamCarving

where

import Control.Monad
import Control.Monad.ST
import Data.Ix
import Data.Array.ST
import Data.Array.Unboxed
import Data.List
import Data.Word

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
    elms = map (\i -> (fst $ seams ! (i, height), (i, height))) [start..width]
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

seamOverlay :: Seam -> RGBImg -> RGBImg
seamOverlay seam image = runSTUArray $ do
  newImage <- newArray size (0 :: Word8)

  forM_ (range size) $ \i -> do
    writeArray newImage i (image ! i)

  forM_ (range (sy, h)) $ \j -> do
    let i = seam ! j
    when (i >= sx && i <= w) $ do
      writeArray newImage (i, j, 0) 255
      writeArray newImage (i, j, 1) 0
      writeArray newImage (i, j, 2) 0
      writeArray newImage (i, j, 3) 255

  return newImage

  where
    size = bounds image
    ((sx, sy, _), (w, h, _)) = size

removeVerticalSeam :: Seam -> RGBImg -> RGBImg
removeVerticalSeam seam image = runSTUArray $ do
  newImage <- newArray (start, (w-1, h, c)) (0 :: Word8)

  forM_ (range (ys, h)) $ \row -> do
    let arow = (seam ! row)

    when (arow >= xs && arow <= w) $ do
      forM_ (range ((xs, zs), (arow, c))) $ \(col, ch) -> do
        writeArray newImage (col, row, ch) (image ! (col, row, ch))

      forM_ (range ((arow, zs), (w - 1, c))) $ \(col, ch) -> do
        writeArray newImage (col, row, ch) (image ! (col + 1, row, ch))

  return newImage

  where
    (start, (w,h,c)) = bounds image
    (xs, ys, zs) = start

transposeImage :: RGBImg -> RGBImg
transposeImage image = runSTUArray $ do
  newImage <- newArray size (0 :: Word8)
  forM_ (range size) $ \idx@(x, y, ch) -> do
    writeArray newImage idx (image ! (y, x, ch))
  return newImage
  where
    ((cx, cy, cch), (w, h, channels)) = bounds image
    size = ((cy, cx, cch), (h, w, channels))

removeVertical :: RGBImg -> RGBImg
removeVertical image = removeVerticalSeam seam image
  where
    en = energy image
    seams = distances en
    m = findMin seams
    seam = minSeam m seams

removeVerticals :: Int -> RGBImg -> RGBImg
removeVerticals !n !image
  | n > 0 = removeVerticals (n - 1) $ removeVertical image
  | otherwise = image

removeHorizontals :: Int -> RGBImg -> RGBImg
removeHorizontals n image = transposeImage $ removeVerticals n $ transposeImage image

