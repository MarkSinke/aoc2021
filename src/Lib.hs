module Lib
    ( day1a, day1b, day2a, day2b, day3a, day3b, day4a, day4b, day5a, day5b, day6a, day6b
    ) where

import Common
import Data.Char (digitToInt)
import Data.List (transpose)
import qualified Data.Map as Map

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

day4a :: IO (Int, [[Int]], [[Bool]])
day4a = do
  (randomNumbers, boards) <- getRandomListAndBoards "/Users/marksinke/IdeaProjects/aoc2021/data/day4input.txt"
  return (getWinningBoard randomNumbers boards)

day4b :: IO (Int, [[Int]], [[Bool]])
day4b = do
  (randomNumbers, boards) <- getRandomListAndBoards "/Users/marksinke/IdeaProjects/aoc2021/data/day4input.txt"
  return (getLosingBoard randomNumbers boards)

getLosingBoard :: [Int] -> [[[Int]]] -> (Int, [[Int]], [[Bool]])
getLosingBoard nums boards =
  let noStrikes =  replicate 5 (replicate 5 False)
  in getLosingBoardX nums boards (repeat noStrikes)

getLosingBoardX :: [Int] -> [[[Int]]] -> [[[Bool]]] -> (Int, [[Int]], [[Bool]])
getLosingBoardX nums boards strikes =
  let num = head nums
      newStrikes = zipWith (strikeBoardDigit num) boards strikes
      newBoards = filter (not . isWinBoard) (zip boards newStrikes)
  in if null newBoards then (num, head boards, head newStrikes)
    else let (recBoards, recStrikes) = unzip newBoards in getLosingBoardX (tail nums) recBoards recStrikes

getWinningBoard :: [Int] -> [[[Int]]] -> (Int, [[Int]], [[Bool]])
getWinningBoard nums boards =
  let noStrikes =  replicate 5 (replicate 5 False)
  in getWinningBoardX nums boards (repeat noStrikes)

getWinningBoardX :: [Int] -> [[[Int]]] -> [[[Bool]]] -> (Int, [[Int]], [[Bool]])
getWinningBoardX nums boards strikes =
  let num = head nums
      newStrikes = zipWith (strikeBoardDigit num) boards strikes
      winBoards = filter isWinBoard (zip boards newStrikes)
  in if not (null winBoards) then (num, fst (head winBoards), snd (head winBoards)) else getWinningBoardX (tail nums) boards newStrikes

strikeBoardDigit :: Int -> [[Int]] -> [[Bool]] -> [[Bool]]
strikeBoardDigit num =
  zipWith (strikeRowDigit num)

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
  in not (null winRows && null winCols)

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

-- DAY5

type VentMap = Map.Map (Int, Int) Int

day5a :: IO Int
day5a = do
  vents <- readVents "/Users/marksinke/IdeaProjects/aoc2021/data/day5input.txt"
  let ventMap = foldr applyVentHV Map.empty vents
  return (length (filter hasVentRisk (Map.toList ventMap)))

day5b :: IO Int
day5b = do
  vents <- readVents "/Users/marksinke/IdeaProjects/aoc2021/data/day5input.txt"
  let ventMap = foldr applyVent Map.empty vents
  return (length (filter hasVentRisk (Map.toList ventMap)))

hasVentRisk ::( (Int, Int), Int) -> Bool
hasVentRisk ((_, _), v) = v >= 2

applyVent :: ((Int, Int), (Int, Int)) -> VentMap -> VentMap
applyVent ((x1, y1), (x2, y2)) ventMap
  | x1 == x2 = applyVentV x1 y1 y2 ventMap
  | y1 == y2 = applyVentH x1 x2 y1 ventMap
  | abs(x1 - x2) == abs(y1 - y2) = applyVentDiag x1 y1 x2 y2 ventMap
  | otherwise = error "nondiag"

applyVentDiag :: Int -> Int -> Int -> Int -> VentMap -> VentMap
applyVentDiag x1 y1 x2 y2 ventMap =
  let dirX = signum (x2 - x1)
      dirY = signum (y2 - y1)
  in foldr applyVentPoint ventMap (zip [x1, x1 + dirX..x2] [y1, y1 + dirY..y2])

applyVentHV :: ((Int, Int), (Int, Int)) -> VentMap -> VentMap
applyVentHV ((x1, y1), (x2, y2)) ventMap
  | x1 == x2 = applyVentV x1 y1 y2 ventMap
  | y1 == y2 = applyVentH x1 x2 y1 ventMap
  | otherwise = ventMap

applyVentH :: Int -> Int -> Int -> VentMap -> VentMap
applyVentH x1 x2 y ventMap =
  foldr applyVentPoint ventMap (zip [(min x1 x2)..(max x1 x2)] (repeat y))

applyVentV :: Int -> Int -> Int -> VentMap -> VentMap
applyVentV x y1 y2 ventMap =
  foldr applyVentPoint ventMap (zip (repeat x) [(min y1 y2)..(max y1 y2)])

applyVentPoint :: (Int, Int) -> VentMap -> VentMap
applyVentPoint coords ventMap =
  let prev = Map.findWithDefault 0 coords ventMap
  in Map.insert coords (prev + 1) ventMap

readVents :: FilePath -> IO [((Int, Int), (Int, Int))]
readVents path = do
    contents <- readFile path
    let myLines = lines contents
    return (map toVent myLines)

toVent :: String -> ((Int, Int), (Int, Int))
toVent str =
  let tokens = words str
  in (readCoord(head tokens), readCoord(tokens !! 2))

readCoord :: String -> (Int, Int)
readCoord str =
  let coords = split ',' str
  in (readInt(head coords), readInt(head(tail coords)))

-- DAY6
day6a :: IO Int
day6a = do
  fish0 <- readLanternFish "/Users/marksinke/IdeaProjects/aoc2021/data/day6input.txt"
  let fish80 = runFish 80 fish0
  return (length fish80)

day6b :: IO Integer
day6b = do
  fish0 <- readLanternFish "/Users/marksinke/IdeaProjects/aoc2021/data/day6input.txt"
  let fishStates = map (countFish fish0) [0..8]
  let fish256 = runFish2 256 fishStates
  return (sum fish256)

countFish :: [Int] -> Int -> Integer
countFish fish n = toInteger (length (filter (== n) fish))

runFish2 :: Int -> [Integer] -> [Integer]
runFish2 daysToGo fish =
  let newFish8 = head fish
      newFish6 = fish !! 7 + newFish8
      newFish7 = fish !! 8
  in if daysToGo == 0 then fish else runFish2 (daysToGo - 1) (take 6 (tail fish) ++ [newFish6, newFish7, newFish8])

runFish :: Int -> [Int] -> [Int]
runFish daysToGo fish =
  if daysToGo == 0 then fish else runFish (daysToGo - 1) (fish >>= nextFishState)

nextFishState :: Int -> [Int]
nextFishState f = if f == 0 then [6, 8] else [f -1]

readLanternFish :: FilePath -> IO [Int]
readLanternFish path = do
    contents <- readFile path
    let myLines = lines contents
    return (splitFish (head myLines))

splitFish :: String -> [Int]
splitFish str =
  let xs = split ',' str
  in map readInt xs
