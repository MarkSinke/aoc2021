module Main where

import Lib (day1a, day1b, day2a, day2b, day3a, day3b, day4a)

main :: IO ()
main = do
  printDay1
  printDay2
  printDay3
  printDay4

printDay1 :: IO ()
printDay1 = do
  putStrLn "Day 1 a"
  putStr "Answer: "
  r1 <- day1a
  print r1
  r2 <- day1b
  putStrLn "Day 1 b"
  print r2

printDay2 :: IO ()
printDay2 = do
  putStrLn "Day 2 a"
  r1 <- day2a
  print r1
  putStr "Answer: "
  print (case r1 of (hor, depth) -> hor * depth)
  r2 <- day2b
  putStrLn "Day 2 b"
  print r2
  putStr "Answer: "
  print (case r2 of (hor, depth, _) -> hor * depth)

printDay3 :: IO ()
printDay3 = do
  putStrLn "Day 3 a"
  r1 <- day3a
  print r1
  putStr "Answer: "
  print (case r1 of (gamma, epsilon) -> gamma * epsilon)
  r2 <- day3b
  putStrLn "Day 3 b"
  print r2
  putStr "Answer: "
  print (case r2 of (oxygen, co2) -> oxygen * co2)

printDay4 :: IO ()
printDay4 = do
  putStrLn "Day 4 a"
  r1 <- day4a
  print r1
  let (num, board, strikes) = r1
  let total = sum(zipWith numOrZeroRow (concat board) (concat strikes))
  putStr "Answer: "
  print (total * num)

numOrZeroRow :: Int -> Bool -> Int
numOrZeroRow x b = if not b then x else 0
