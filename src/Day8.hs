module Day8 (day8a, day8b)
where

import Common
import Data.Maybe (fromJust, fromMaybe)
import Data.Foldable (find)
import Data.List (isInfixOf, findIndex, sort, elemIndex)
import Debug.Trace (trace)

day8a :: IO Int
day8a = do
  inOut <- readSignalFile "/Users/marksinke/IdeaProjects/aoc2021/data/day8input.txt"
  let outs = concatMap snd inOut
  return (length (filter isUniqueDigit outs))

day8b :: IO Int
day8b = do
  inOut <- readSignalFile "/Users/marksinke/IdeaProjects/aoc2021/data/day8input.txt"
  let values = map deduceOut inOut
  return (sum values)

-- number length how to find
-- 0      6      length 6 and not a 6 or 9
-- 1      2      only one length 2
-- 2      5
-- 3      5
-- 4      4      only one length 4
-- 5      5      length 5 and subset of 9
-- 6      6      length 6 and 1 is not a subset
-- 7      3      only one length 3
-- 8      7      only one length 7
-- 9      6      length 6 and 4 is a subset
deduceOut :: ([String], [String]) -> Int
deduceOut (inputs, outputs) =
  let signal1 = fromJust (find (\x -> length x == 2) inputs)
      signal4 = fromJust (find (\x -> length x == 4) inputs)
      signal7 = fromJust (find (\x -> length x == 3) inputs)
      signal8 = fromJust (find (\x -> length x == 7) inputs)
      signal6 = fromJust (find (\x -> length x == 6 && not (containsAll signal1 x)) inputs)
      signal9 = fromJust (find (\x -> length x == 6 && containsAll signal4 x) inputs)
      signal0 = fromJust (find (\x -> length x == 6 && x /= signal6 && x /= signal9) inputs)
      signal3 = fromJust (find (\x -> length x == 5 && containsAll signal1 x) inputs)
      signal5 = fromJust (find (\x -> length x == 5 && containsAll x signal9 && x /= signal3) inputs)
      signal2 = fromJust (find (\x -> length x == 5 && x /= signal5 && x /= signal3) inputs)
      signals = map sort [signal0, signal1, signal2, signal3, signal4, signal5, signal6, signal7, signal8, signal9]
      sortedOut = map sort outputs
      nums = map (\x -> fromJust (elemIndex x signals)) sortedOut
      num = foldl (\a x -> a * 10 + x) 0 nums
   in num

containsAll :: String -> String -> Bool
containsAll search str =
  all (`elem` str) search

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

