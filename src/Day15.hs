module Day15 (day15a, day15b) where

import Data.Char (digitToInt)
import Algorithm.Search (dijkstra)
import Data.Maybe (fromJust)

day15a :: IO Int
day15a = do
  riskMatrix <- readRiskMatrix "/Users/marksinke/IdeaProjects/aoc2021/data/day15input.txt"
  let maxX = length (head riskMatrix)
  let maxY = length riskMatrix
  let start = (0, 0)
  let (cost, _) = fromJust (dijkstra (nextCell maxX maxY) (cellCost riskMatrix) (isEndState maxX maxY) start)
  return cost

day15b :: IO Int
day15b = do
  riskMatrix <- readRiskMatrix "/Users/marksinke/IdeaProjects/aoc2021/data/day15input.txt"
  let maxX = length (head riskMatrix)
  let maxY = length riskMatrix
  let start = (0, 0)
  let (cost, _) = fromJust (dijkstra (nextCell (maxX * 5) (maxY * 5)) (cellCost5 maxX maxY riskMatrix) (isEndState (maxX * 5) (maxY * 5)) start)
  return cost

nextCell :: Int -> Int -> (Int, Int) -> [(Int, Int)]
nextCell maxX maxY (x, y) =
  filter (isValidCell maxX maxY) [(x, y -1), (x - 1, y), (x + 1, y), (x, y + 1)]

cellCost :: [[Int]] -> (Int, Int) -> (Int, Int) -> Int
cellCost riskMatrix _ (x, y) = (riskMatrix !! y) !! x

cellCost5 :: Int -> Int -> [[Int]] -> (Int, Int) -> (Int, Int) -> Int
cellCost5 maxX maxY riskMatrix _ (x, y) =
  let qX = div x maxX
      qY = div y maxY
      iX = mod x maxX
      iY = mod y maxY
      offset = qX + qY
      val = (riskMatrix !! iY) !! iX + offset
      clippedVal = mod (val - 1) 9 + 1
  in clippedVal

isValidCell :: Int -> Int -> (Int, Int) -> Bool
isValidCell maxX maxY (x, y) =
  x >= 0 && x < maxX && y >= 0 && y < maxY

isEndState :: Int -> Int -> (Int, Int) -> Bool
isEndState maxX maxY (x, y) =
  x == maxX - 1 && y == maxY - 1

readRiskMatrix :: FilePath -> IO [[Int]]
readRiskMatrix path = do
    contents <- readFile path
    let myLines = lines contents
    let ints = map getDigits myLines
    return ints

getDigits :: [Char] -> [Int]
getDigits = map digitToInt

