module Day11 (day11a, day11b)
where

import qualified Data.Matrix as Matrix
import Data.Char (digitToInt)
import Debug.Trace (trace)
import Common

day11a :: IO Int
day11a = do
  energy <- readEnergy "/Users/marksinke/IdeaProjects/aoc2021/data/day11input.txt"
  let (flashes, _) = runFlashes 100 0 energy
  return flashes

day11b :: IO Int
day11b = do
  energy <- readEnergy "/Users/marksinke/IdeaProjects/aoc2021/data/day11input.txt"
  let flashes = runUntilAllFlash 1 energy
  return flashes

runUntilAllFlash :: Int -> Matrix.Matrix Int -> Int
runUntilAllFlash steps input =
  let (flashes, newInput) = runFlash input
  in
    if flashes == 100 then steps
    else runUntilAllFlash (steps + 1) newInput

runFlashes :: Int -> Int -> Matrix.Matrix Int -> (Int, Matrix.Matrix Int)
runFlashes steps flashCount input =
  if steps == 0 then (flashCount, input)
  else let (newFlash, newInput) = runFlash input
    in runFlashes (steps - 1) (flashCount + newFlash) newInput

runFlash :: Matrix.Matrix Int -> (Int, Matrix.Matrix Int)
runFlash input =
  let coords = Matrix.toList (Matrix.matrix (Matrix.nrows input) (Matrix.ncols input) id)
      flashed = foldl flashPos input coords
      newMatrix = fmap cleanEnergy flashed
  in (length (filter (==0) (Matrix.toList newMatrix)), newMatrix)

cleanEnergy :: Int -> Int
cleanEnergy a = if a < 10 then a else 0

flashPos :: Matrix.Matrix Int -> (Int, Int) -> Matrix.Matrix Int
flashPos input (r, c) =
  let oldValue = Matrix.getElem r c input
      newValue = oldValue + 1
      newMatrix = Matrix.setElem newValue (r, c) input
  in if newValue == 10 then flashAround newMatrix (r, c)  else newMatrix

flashAround :: Matrix.Matrix Int -> (Int, Int) -> Matrix.Matrix Int
flashAround input (r, c) =
  (flashSafe (r - 1, c - 1) . flashSafe (r - 1, c) . flashSafe (r - 1, c + 1) .
  flashSafe (r, c -1) . flashSafe (r, c + 1) .
  flashSafe (r + 1, c - 1) . flashSafe (r + 1, c) . flashSafe (r + 1, c + 1)) input

flashSafe :: (Int, Int) -> Matrix.Matrix Int -> Matrix.Matrix Int
flashSafe (r, c) input =
  if isValidCell (r, c) input then flashPos input (r, c) else input

isValidCell :: (Int, Int) -> Matrix.Matrix Int -> Bool
isValidCell (r, c) matrix =
  c >= 1 &&  c <= Matrix.ncols matrix && r >= 1 && r <= Matrix.nrows matrix

readEnergy :: FilePath -> IO (Matrix.Matrix Int)
readEnergy path = do
  contents <- readFile path
  let myLines = lines contents
  let mat = map toEnergyLine myLines
  return (Matrix.fromLists mat)

toEnergyLine :: [Char] -> [Int]
toEnergyLine =
  map digitToInt

