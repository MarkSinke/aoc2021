module Day10 (day10a, day10b)
where

import Data.Maybe (catMaybes)


day10a :: IO Int
day10a = do
  navs <- readNavigation "/Users/marksinke/IdeaProjects/aoc2021/data/day10input.txt"
  let corrupted = map getCorruptChar navs
  return (foldl computeScore 0 (catMaybes corrupted))

computeScore :: Int -> Char -> Int
computeScore a x = a + case x of
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137

day10b :: IO Int
day10b = do
  heights <- readNavigation "/Users/marksinke/IdeaProjects/aoc2021/data/day9input.txt"
  return 0

readNavigation :: FilePath -> IO [String]
readNavigation path = do
  contents <- readFile path
  return (lines contents)

getCorruptChar :: String -> Maybe Char
getCorruptChar = getCorruptCharRec []

getCorruptCharRec :: [Char] -> [Char] -> Maybe Char
getCorruptCharRec stack ('(' : xs) = getCorruptCharRec ('(' : stack) xs
getCorruptCharRec stack ('{' : xs) = getCorruptCharRec ('{' : stack) xs
getCorruptCharRec stack ('[' : xs) = getCorruptCharRec ('[' : stack) xs
getCorruptCharRec stack ('<' : xs) = getCorruptCharRec ('<' : stack) xs
getCorruptCharRec ('(' : stack) (')' : xs) = getCorruptCharRec stack xs
getCorruptCharRec ('{' : stack) ('}' : xs) = getCorruptCharRec stack xs
getCorruptCharRec ('[' : stack) (']' : xs) = getCorruptCharRec stack xs
getCorruptCharRec ('<' : stack) ('>' : xs) = getCorruptCharRec stack xs
getCorruptCharRec (top : stack) [] = Nothing -- incomplete
getCorruptCharRec _ [] = Nothing -- complete and correct
getCorruptCharRec _ (x : xs) = Just x -- corrupt and x is the offending character
