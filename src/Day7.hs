module Day7 (day7a, day7b)
where

import Common

day7a :: IO Int
day7a = do
  nums <- readInts "/Users/marksinke/IdeaProjects/aoc2021/data/day7input.txt"
  let maxPos = maximum nums
  let costs = map (computeDelta computeCost nums) [0..maxPos]
  return (minimum costs)

computeDelta :: (Int -> Int -> Int) -> [Int] -> Int -> Int
computeDelta f xs pos = sum (map (f pos) xs)

computeCost :: Int -> Int -> Int
computeCost pos x = abs(pos - x)

computeCost2 :: Int -> Int -> Int
computeCost2 pos x =
  let d = abs(pos - x)
  in d * (d + 1) `div` 2

readInts :: FilePath -> IO [Int]
readInts path = do
  contents <- readFile path
  let numStr = head (lines contents)
  return (map readInt (split ',' numStr))


day7b :: IO Int
day7b = do
  nums <- readInts "/Users/marksinke/IdeaProjects/aoc2021/data/day7input.txt"
  let maxPos = maximum nums
  let costs = map (computeDelta computeCost2 nums) [0..maxPos]
  return (minimum costs)
