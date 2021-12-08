module Day8 (day8a)
where

import Common

day8a :: IO Int
day8a = do
  inOut <- readSignalFile "/Users/marksinke/IdeaProjects/aoc2021/data/day8input.txt"
  let outs = concatMap snd inOut
  return (length (filter isUniqueDigit outs))

isUniqueDigit :: String -> Bool
isUniqueDigit str =
  let len = length str
  in len == 2 || len == 3 || len == 4 || len == 7

readSignalFile :: FilePath -> IO [([String], [String])]
readSignalFile path = do
  contents <- readFile path
  let myLines = lines contents
  return (map toInOut myLines)

toInOut :: String -> ([String], [String])
toInOut str =
  let inOut = split '|' str
      input = head inOut
      output = head (tail inOut)
  in (words input, words output)

