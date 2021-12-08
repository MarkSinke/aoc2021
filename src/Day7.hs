module Day7 (day7a, day7b)
where

import Common

day7a :: IO Int
day7a = do
  nums <- readInts "/Users/marksinke/IdeaProjects/aoc2021/data/day7input.txt"
  let maxPos = maximum nums
  let costs = map (computeDelta nums) [0..maxPos]
  return (minimum costs)

computeDelta :: [Int] -> Int -> Int
computeDelta xs pos = sum (map (computeCost pos) xs)

computeCost :: Int -> Int -> Int
computeCost pos x = abs(pos - x)

readInts :: FilePath -> IO [Int]
readInts path = do
  contents <- readFile path
  let numStr = head (lines contents)
  return (map readInt (split ',' numStr))


day7b :: IO ()
day7b = do
  return ()