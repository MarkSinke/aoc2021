module Day14(day14a, day14b) where

import Data.Foldable (find)
import Data.Maybe (fromJust)
import Data.List (sort, group)

day14a :: IO Int
day14a = do
  (polymer, rules) <- readPolymerFile "/Users/marksinke/IdeaProjects/aoc2021/data/day14input.txt"

  let polymer10 = nTimes 10 (substitute rules) polymer
  let sorted = sort polymer10
  let grouped = group sorted
  let lengths = map length grouped
  let sortedLengths = sort lengths
  return (last sortedLengths - head sortedLengths)

nTimes :: Int -> (String -> String) -> String -> String
nTimes 0 f a = a
nTimes steps f a = nTimes (steps - 1) f (f a)

substitute :: [(String, Char)] -> String -> String
substitute rules (a : b : xs) =
  let ins = findPair rules a b
  in a : ins : substitute rules (b : xs)
substitute rules x = x

findPair :: [(String, Char)] -> Char -> Char -> Char
findPair rules a b =
  snd (fromJust (find (\x -> fst x == a : [b]) rules))

day14b :: IO Int
day14b = do
  return 0

readPolymerFile :: FilePath -> IO (String, [(String, Char)])
readPolymerFile path = do
  contents <- readFile path
  let myLines = lines contents
  let polymer = head myLines
  let rules = drop 2 myLines
  return (polymer, map toRule rules)

toRule :: String -> (String, Char)
toRule str =
  let parts = words str
  in (head parts, head (parts !! 2))
