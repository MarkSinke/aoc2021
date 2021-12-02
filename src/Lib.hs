module Lib
    ( day1a, day1b, day2a, day2b
    ) where

-- DAY1

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

-- DAY2

day2a = do
  commands <- getCommands "/Users/marksinke/IdeaProjects/aoc2021/data/day2input.txt"
  return (applyCommands commands)

day2b = "LATER"

applyCommands:: [(String, Int)] -> (Int, Int)
applyCommands commands =
  let initialPos = (0, 0)
  in foldl applyCommand (initialPos) commands

applyCommand :: (Int, Int) -> (String, Int) -> (Int, Int)
applyCommand (hor, depth) command =
  let (verb, num) = command in
  case verb of
    "forward" -> (hor + num, depth)
    "backward" -> (hor - num, depth)
    "up" -> (hor, depth - num)
    "down" -> (hor, depth + num)

getCommands :: FilePath -> IO [(String, Int)]
getCommands path = do
    contents <- readFile path
    let myLines = lines contents
    let someFloats = map readCommand myLines
    return someFloats

readCommand :: String -> (String, Int)
readCommand str =
  let myWords = words str
  in (myWords !! 0, read (myWords !! 1))