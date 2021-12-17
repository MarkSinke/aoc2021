module Day10 (day10a, day10b)
where

import Data.Maybe (isNothing, mapMaybe)
import Data.List (sort)

day10a :: IO Int
day10a = do
  navs <- readNavigation "/Users/marksinke/IdeaProjects/aoc2021/data/day10input.txt"
  let parsed = map getCorruptChar navs
  return (foldl computeScore 0 (mapMaybe snd parsed))

computeScore :: Int -> Char -> Int
computeScore a x = a + case x of
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137
  _ -> error "bad syntax"

day10b :: IO Int
day10b = do
  navs <- readNavigation "/Users/marksinke/IdeaProjects/aoc2021/data/day10input.txt"
  let parsed = map getCorruptChar navs
  let incomplete = map fst (filter (isNothing . snd) parsed)
  let scores = map toScore incomplete
  let sorted = sort scores
  let middleIndex = div (length sorted) 2
  return (sorted !! middleIndex)

toScore :: String -> Int
toScore = foldl scoreChar 0

scoreChar :: Int -> Char -> Int
scoreChar s c =
  s * 5 + (case c of
    '(' -> 1
    '[' -> 2
    '{' -> 3
    '<' -> 4
    _ -> 0)

readNavigation :: FilePath -> IO [String]
readNavigation path = do
  contents <- readFile path
  return (lines contents)

getCorruptChar :: String -> ([Char], Maybe Char)
getCorruptChar = getCorruptCharRec []

getCorruptCharRec :: [Char] -> [Char] -> ([Char], Maybe Char)
getCorruptCharRec stack ('(' : xs) = getCorruptCharRec ('(' : stack) xs
getCorruptCharRec stack ('{' : xs) = getCorruptCharRec ('{' : stack) xs
getCorruptCharRec stack ('[' : xs) = getCorruptCharRec ('[' : stack) xs
getCorruptCharRec stack ('<' : xs) = getCorruptCharRec ('<' : stack) xs
getCorruptCharRec ('(' : stack) (')' : xs) = getCorruptCharRec stack xs
getCorruptCharRec ('{' : stack) ('}' : xs) = getCorruptCharRec stack xs
getCorruptCharRec ('[' : stack) (']' : xs) = getCorruptCharRec stack xs
getCorruptCharRec ('<' : stack) ('>' : xs) = getCorruptCharRec stack xs
getCorruptCharRec stack (x : _) = (stack, Just x) -- corrupt and x is the offending character
getCorruptCharRec stack [] = (stack, Nothing) -- incomplete, or correct (correct iff stack is empty)
