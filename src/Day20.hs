module Day20 (day20a, day20b) where

import Data.Maybe (fromMaybe)
import qualified Data.Matrix as Matrix
import qualified Data.Vector.Unboxed as Unbox

data Image = Image Bool (Matrix.Matrix Bool)

instance Show Image where
  show = fromImage

day20a :: IO Int
day20a = do
  (transform, image) <- readImageFile "/Users/marksinke/IdeaProjects/aoc2021/data/day20input.txt"

  let image1 = enhance transform image
  let image2 = enhance transform image1

  return (countOn image2)

day20b :: IO Int
day20b = do
  (transform, image) <- readImageFile "/Users/marksinke/IdeaProjects/aoc2021/data/day20input.txt"

  let result = iterate (enhance transform) image !! 50

  return (countOn result)

countOn :: Image -> Int
countOn (Image True _) = error "cannot count - universe is lit"
countOn (Image False m) = foldl countOnM 0 m

countOnM :: Int -> Bool -> Int
countOnM acc True = acc + 1
countOnM acc False = acc

readImageFile :: FilePath -> IO (Unbox.Vector Bool, Image)
readImageFile path = do
  contents <- readFile path
  let myLines = lines contents
  let transform = Unbox.fromList(map toBool (head myLines))
  let image = Matrix.fromLists (map toBools (drop 2 myLines))
  return (transform, Image False image)

toBool :: Char -> Bool
toBool '#' = True
toBool '.' = False
toBool x = error (x :": bad input")

fromBool :: Bool -> Char
fromBool b = if b then '#' else '.'

toBools :: [Char] -> [Bool]
toBools = map toBool

fromImage :: Image -> String
fromImage (Image b m) =
  let lists = Matrix.toLists m
  in "--- Universe: " ++ [fromBool b] ++ "\n" ++ concatMap (\x -> map fromBool x ++ "\n") lists

enhance :: Unbox.Vector Bool -> Image -> Image
enhance transform (Image universePixel input) =
  let pixels = Matrix.matrix (Matrix.nrows input + 4) (Matrix.ncols input + 4) (doPixel universePixel transform input)
      newUniverse = transform Unbox.! (if universePixel then 511 else 0)
  in Image newUniverse pixels

doPixel :: Bool -> Unbox.Vector Bool -> Matrix.Matrix Bool -> (Int, Int) -> Bool
doPixel universePixel transform input (row, col) =
  let oldRow = row - 2
      oldCol = col - 2
      bits = map (getPixel universePixel input (oldRow, oldCol)) offsets
      index = toInt bits
  in transform Unbox.! index

toInt :: [Bool] -> Int
toInt = foldl (\acc x -> 2 * acc + (if x then 1 else 0)) 0

offsets :: [(Int, Int)]
offsets = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 0), (0, 1), (1, -1), (1, 0), (1, 1)]

getPixel :: Bool -> Matrix.Matrix Bool -> (Int, Int) -> (Int, Int) -> Bool
getPixel universePixel input (row, col) (rowOffset, colOffset) =
  let rowActual = row + rowOffset
      colActual = col + colOffset
      val = Matrix.safeGet rowActual colActual input
  in fromMaybe universePixel val