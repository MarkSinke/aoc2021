module Lib
    ( day1a, day1b, day2a, day2b, day3a, day3b, day4a
    ) where
import Data.Char (digitToInt)
import Data.List (transpose)
import Debug.Trace (trace)

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

-- DAY3

day3a :: IO (Int, Int)
day3a = do
  inputs <- getBinaryStrings "/Users/marksinke/IdeaProjects/aoc2021/data/day3input.txt"
  let inputsPerBit = transpose inputs
  let mostCommonDigits = map getMostCommonDigit inputsPerBit
  let leastCommonDigits = map invertBit mostCommonDigits
  return (createBinaryNumber mostCommonDigits, createBinaryNumber leastCommonDigits)

day3b :: IO (Int, Int)
day3b = do
  inputs <- getBinaryStrings "/Users/marksinke/IdeaProjects/aoc2021/data/day3input.txt"
  let oxygenResult = reduceBasedOnFirstDigit getMostCommonDigit [] inputs
  let co2Result = reduceBasedOnFirstDigit getLeastCommonDigit [] inputs
  let oxygen = createBinaryNumber(fst oxygenResult)
  let co2 = createBinaryNumber(fst co2Result)
  return (oxygen, co2)

reduceBasedOnFirstDigit :: ([Int] -> Int) -> [Int] -> [[Int]] -> ([Int], [[Int]])
reduceBasedOnFirstDigit selector result inputs =
  if null (head inputs) then (result, inputs)
  else
    let filterDigit = selector (firstBits inputs)
        filtered = filter (\x -> head x == filterDigit) inputs
        tails = map tail filtered
        newResult = result ++ [filterDigit]
    in reduceBasedOnFirstDigit selector newResult tails

firstBits :: [[Int]] -> [Int]
firstBits = map head

invertBit :: Int -> Int
invertBit x = 1 - x

getMostCommonDigit :: [Int] -> Int
getMostCommonDigit xs =
  let (zeros, ones) = countZerosAndOnes xs
  in if zeros > 0 && (ones == 0 || zeros > ones) then 0 else 1

getLeastCommonDigit :: [Int] -> Int
getLeastCommonDigit xs =
  let (zeros, ones) = countZerosAndOnes xs
  in if zeros > 0 && (ones == 0 || zeros <= ones) then 0 else 1

countZerosAndOnes :: [Int] -> (Int, Int)
countZerosAndOnes =
  foldr (\x (zeros, ones) -> if x == 0 then (zeros + 1, ones) else (zeros, ones + 1)) (0, 0)

getBinaryStrings :: FilePath -> IO [[Int]]
getBinaryStrings path = do
    contents <- readFile path
    let myLines = lines contents
    let ints = map getBinaryString myLines
    return ints

getBinaryString :: [Char] -> [Int]
getBinaryString = map digitToInt

createBinaryNumber :: [Int] -> Int
createBinaryNumber =
  foldl (\a x -> x + a * 2) 0

-- DAY 4

day4a :: IO (Int, Int)
day4a = do
  (randomNumbers, boards) <- getRandomListAndBoards "/Users/marksinke/IdeaProjects/aoc2021/data/day4input.txt"
  let (num, board, strikes) = getWinningBoard randomNumbers boards
  print num
  print board
  print strikes
  return (0,0)

getWinningBoard :: [Int] -> [[[Int]]] -> (Int, [[Int]], [[Bool]])
getWinningBoard nums boards =
  let noStrikes =  repeat (replicate 5 (replicate 5 False))
  in getWinningBoardX nums boards noStrikes
  
getWinningBoardX :: [Int] -> [[[Int]]] -> [[[Bool]]] -> (Int, [[Int]], [[Bool]])
getWinningBoardX nums boards strikes =
  let num = head nums
      newStrikes = zipWith (strikeBoardDigit num) boards strikes
      winBoards = filter isWinBoard (zip boards newStrikes)
  in trace("num = " ++ show num ++ " strikes" ++ show strikes) (if null winBoards then getWinningBoardX (tail nums) boards newStrikes else (num, fst(head winBoards), snd(head winBoards)))

strikeBoardDigit :: Int -> [[Int]] -> [[Bool]] -> [[Bool]]
strikeBoardDigit num boards strikes =
  trace("try striking " ++ show num ++ "in boards " ++ show boards) (zipWith (strikeRowDigit num) boards strikes)

strikeRowDigit :: Int -> [Int] -> [Bool] -> [Bool]
strikeRowDigit num =
  zipWith (strikeDigit num)

strikeDigit :: Int -> Int -> Bool -> Bool
strikeDigit num boardDigit strike =
  strike || (num == boardDigit)

isWinBoard :: ([[Int]], [[Bool]]) -> Bool
isWinBoard (_, strikes) =
  let winRows = filter isStrikeRow strikes
      winCols = filter isStrikeRow (transpose strikes)
  in not (null winRows || null winCols)

isStrikeRow :: [Bool] -> Bool
isStrikeRow = and

getRandomListAndBoards :: FilePath -> IO ([Int], [[[Int]]])
getRandomListAndBoards path = do
    contents <- readFile path
    let myLines = lines contents
    let randomNumbers = readRandomInts (head myLines)
    let boardsStr = tail (tail myLines)
    let boards = map toBoard (chunks 6 boardsStr)
    return (randomNumbers, boards)

readRandomInts :: String -> [Int]
readRandomInts str = map readInt (split ',' str)

toBoard :: [String] -> [[Int]]
toBoard xs =
  take 5 (map toBoardLine xs)

toBoardLine :: String -> [Int]
toBoardLine str = map readInt (words str)

readInt :: String -> Int
readInt = read

split :: Char -> String -> [String]
split _ "" = []
split delimiter str =
    let (start, rest) = break (== delimiter) str
        (_, remain) = span (== delimiter) rest
     in start : split delimiter remain

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs