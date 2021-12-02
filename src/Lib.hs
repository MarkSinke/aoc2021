module Lib
    ( day1
    ) where

day1 = do
  nums <- getInts "/Users/marksinke/IdeaProjects/aoc2021/data/day1input.txt"
  let pairs = (zip nums (drop 1 nums))
  return (length (filter increasing pairs))

increasing :: (Int, Int ) -> Bool
increasing tuple =
  (fst tuple) < (snd tuple)

getInts :: FilePath -> IO [Int]
getInts path = do
  contents <- readFile path
  let someFloats = map read  . lines $ contents
  return someFloats