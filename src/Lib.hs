module Lib
    ( day1a, day1b
    ) where

day1a = do
  nums <- getInts "/Users/marksinke/IdeaProjects/aoc2021/data/day1input.txt"
  let pairs = (zip nums (drop 1 nums))
  return (length (filter increasing pairs))

day1b = do
  nums <- getInts "/Users/marksinke/IdeaProjects/aoc2021/data/day1input.txt"
  let windows = (zip3 nums (drop 1 nums) (drop 2 nums))
  let sums = map sum3 windows
  let pairs = (zip sums (drop 1 sums))
  return (length (filter increasing pairs))

sum3 (a, b, c) =
  a + b + c

increasing (a, b) =
  a < b

getInts :: FilePath -> IO [Int]
getInts path = do
  contents <- readFile path
  let someFloats = map read  . lines $ contents
  return someFloats