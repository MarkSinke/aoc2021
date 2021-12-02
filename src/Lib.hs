module Lib
    ( day1a, day1b, day2a, day2b
    ) where

-- DAY1

day1a :: IO Int
day1a = do
  nums <- getInts "/Users/marksinke/IdeaProjects/aoc2021/data/day1input.txt"
  let pairs = zip nums (drop 1 nums)
  return (length (filter increasing pairs))

day1b :: IO Int
day1b = do
  nums <- getInts "/Users/marksinke/IdeaProjects/aoc2021/data/day1input.txt"
  let windows = zip3 nums (drop 1 nums) (drop 2 nums)
  let sums = map sum3 windows
  let pairs = zip sums (drop 1 sums)
  return (length (filter increasing pairs))

sum3 :: (Int, Int, Int) -> Int
sum3 (a, b, c) =
  a + b + c

increasing :: (Int, Int) -> Bool
increasing (a, b) =
  a < b

getInts :: FilePath -> IO [Int]
getInts path = do
  contents <- readFile path
  let someFloats = map read  . lines $ contents
  return someFloats

-- DAY2

day2a :: IO (Int, Int)
day2a = do
  commands <- getCommands "/Users/marksinke/IdeaProjects/aoc2021/data/day2input.txt"
  let initialPos = (0, 0)
  return (foldl applyCommandA initialPos commands)

day2b :: IO (Int, Int, Int)
day2b = do
  commands <- getCommands "/Users/marksinke/IdeaProjects/aoc2021/data/day2input.txt"
  let initialPos = (0, 0, 0)
  return (foldl applyCommandB initialPos commands)

applyCommandA :: (Int, Int) -> (String, Int) -> (Int, Int)
applyCommandA (hor, depth) command =
  let (verb, num) = command in
  case verb of
    "forward" -> (hor + num, depth)
    "backward" -> (hor - num, depth)
    "up" -> (hor, depth - num)
    "down" -> (hor, depth + num)
    _ -> error "failed to parse input"

applyCommandB :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
applyCommandB (hor, depth, aim) command =
  let (verb, num) = command in
  case verb of
    "forward" -> (hor + num, depth + aim * num, aim)
    "backward" -> (hor - num, depth + aim * num, aim)
    "up" -> (hor, depth, aim - num)
    "down" -> (hor, depth, aim + num)
    _ -> error "failed to parse input"

getCommands :: FilePath -> IO [(String, Int)]
getCommands path = do
    contents <- readFile path
    let myLines = lines contents
    return (map readCommand myLines)

readCommand :: String -> (String, Int)
readCommand str =
  case words str of
     command : numStr : _ -> (command, read numStr)
     _ -> error "failed to parse input"
