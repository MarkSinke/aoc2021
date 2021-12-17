module Day14(day14a, day14b) where

import Data.Maybe (fromJust)
import Data.List (sort, group, sortOn)
import qualified Data.Map as Map
import Data.Map.Strict (insertWith)

day14a :: IO Int
day14a = do
  (polymer, ruleMap) <- readPolymerFile "/Users/marksinke/IdeaProjects/aoc2021/data/day14input.txt"

  return (runSimulation 10 polymer ruleMap)

runSimulation :: Int -> String -> Map.Map String Char -> Int
runSimulation steps polymer ruleMap =
  let polymerPairs = toPairs polymer
      pairCounts = foldl (\acc x -> insertWith (+) x 1 acc) Map.empty polymerPairs
      letterGroups = group (sort polymer)
      letterCounts = Map.fromList (map toCharCount letterGroups)
      (_, lettersN) = iterate (substitutePairs ruleMap) (pairCounts, letterCounts) !! steps
      sortedLetters = sortOn snd (Map.toList lettersN)
  in snd (last sortedLetters) - snd (head sortedLetters)

toCharCount :: String -> (Char, Int)
toCharCount str = (head str, length str)

day14b :: IO Int
day14b = do
  (polymer, ruleMap) <- readPolymerFile "/Users/marksinke/IdeaProjects/aoc2021/data/day14input.txt"

  return (runSimulation 40 polymer ruleMap)

toPairs :: String -> [String]
toPairs (a : b : xs) =
  [a, b] : toPairs (b : xs)
toPairs _ = []

substitutePairs :: Map.Map String Char -> (Map.Map String Int, Map.Map Char Int) -> (Map.Map String Int, Map.Map Char Int)
substitutePairs pairRules (pairCounts, letterCounts) =
  Map.foldrWithKey (doPair pairRules) (Map.empty, letterCounts) pairCounts

doPair :: Map.Map String Char -> String -> Int -> (Map.Map String Int, Map.Map Char Int) -> (Map.Map String Int, Map.Map Char Int)
doPair pairRules pair count (pairCounts, letterCounts) =
  let (a, b) = splitPair pair
      c = fromJust (Map.lookup pair pairRules)
  in (insertWith (+) [a, c] count (insertWith (+) [c, b] count pairCounts), insertWith (+) c count letterCounts)

splitPair :: String -> (Char, Char)
splitPair [a, b] = (a, b)
splitPair _ = error "bad pair"

readPolymerFile :: FilePath -> IO (String, Map.Map String Char)
readPolymerFile path = do
  contents <- readFile path
  let myLines = lines contents
  let polymer = head myLines
  let rules = drop 2 myLines
  return (polymer, Map.fromList (map toRule rules))

toRule :: String -> (String, Char)
toRule str =
  let parts = words str
  in (head parts, head (parts !! 2))
