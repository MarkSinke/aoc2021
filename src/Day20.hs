module Day20 (day20a, day20b) where

import Data.Maybe (fromMaybe)
import qualified Data.Matrix as Matrix
import qualified Data.Vector.Unboxed as Unbox

day20a :: IO Int
day20a = do
  (transform, image) <- readImageFile "/Users/marksinke/IdeaProjects/aoc2021/data/day20input.txt"

  let image1 = enhance transform image
  let image2 = enhance transform image1

  return (foldl countOn 0 image2)

day20b :: IO Int
day20b = do
  return 0

countOn :: Int -> Bool -> Int
countOn acc x = acc + (if x then 1 else 0)

readImageFile :: FilePath -> IO (Unbox.Vector Bool, Matrix.Matrix Bool)
readImageFile path = do
  contents <- readFile path
  let myLines = lines contents
  let transform = Unbox.fromList(map toBool (head myLines))
  let image = Matrix.fromLists (map toBools (drop 2 myLines))
  return (transform, image)

toBool :: Char -> Bool
toBool '#' = True
toBool '.' = False
toBool x = error (x :": bad input")

fromBool :: Bool -> Char
fromBool b = if b then '#' else '.'

toBools :: [Char] -> [Bool]
toBools = map toBool

toImage :: Matrix.Matrix Bool -> String
toImage m =
  let lists = Matrix.toLists m
  in concatMap (\x -> map fromBool x ++ "\n") lists

enhance :: Unbox.Vector Bool -> Matrix.Matrix Bool -> Matrix.Matrix Bool
enhance transform input =
  Matrix.matrix (Matrix.nrows input + 4) (Matrix.ncols input + 4) (doPixel transform input)

doPixel :: Unbox.Vector Bool -> Matrix.Matrix Bool -> (Int, Int) -> Bool
doPixel transform input (row, col) =
  let oldRow = row - 2
      oldCol = col - 2
      bits = map (getPixel input (oldRow, oldCol)) offsets
      index = toInt bits
  in transform Unbox.! index

toInt :: [Bool] -> Int
toInt = foldl (\acc x -> 2 * acc + (if x then 1 else 0)) 0

offsets :: [(Int, Int)]
offsets = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 0), (0, 1), (1, -1), (1, 0), (1, 1)]

getPixel :: Matrix.Matrix Bool -> (Int, Int) -> (Int, Int) -> Bool
getPixel input (row, col) (rowOffset, colOffset) =
  let rowActual = row + rowOffset
      colActual = col + colOffset
      val = Matrix.safeGet rowActual colActual input
  in fromMaybe False val