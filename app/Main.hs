module Main where

import Lib (day1a, day1b, day2a, day2b, day3a, day3b, day4a, day4b, day5a, day5b, day6a, day6b)
import Day7 (day7a, day7b)
import Day8 (day8a, day8b)
import Day9 (day9a, day9b)
import Day10 (day10a, day10b)

main :: IO ()
main = do
--  printDay1
--  printDay2
--  printDay3
--  printDay4
--  printDay5
--  printDay6
--  printDay7
--  printDay8
--  printDay9
  printDay10

printDay1 :: IO ()
printDay1 = do
  putStrLn "Day 1 a"
  r1 <- day1a
  print r1

  putStrLn "Day 1 b"
  r2 <- day1b
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

  putStrLn "Day 4 b"
  r2 <- day4b
  print r2
  let (num2, board2, strikes2) = r2
  let total2 = sum(zipWith numOrZeroRow (concat board2) (concat strikes2))
  putStr "Answer: "
  print (total2 * num2)

numOrZeroRow :: Int -> Bool -> Int
numOrZeroRow x b = if not b then x else 0

printDay5 :: IO ()
printDay5 = do
  putStrLn "Day 5 a"
  r1 <- day5a
  print r1

  putStrLn "Day 5 b"
  r2 <- day5b
  print r2

printDay6 :: IO ()
printDay6 = do
  putStrLn "Day 6 a"
  r1 <- day6a
  print r1

  putStrLn "Day 6 b"
  r2 <- day6b
  print r2

printDay7 :: IO ()
printDay7 = do
  putStrLn "Day 7 a"
  r1 <- day7a
  print r1

  putStrLn "Day 7 b"
  r2 <- day7b
  print r2

printDay8 :: IO ()
printDay8 = do
  putStrLn "Day 8 a"
  r1 <- day8a
  print r1

  putStrLn "Day 8 b"
  r2 <- day8b
  print r2

printDay9 :: IO ()
printDay9 = do
  putStrLn "Day 9 a"
  r1 <- day9a
  print r1

  putStrLn "Day 9 b"
  r2 <- day9b
  print r2

printDay10 :: IO ()
printDay10 = do
  putStrLn "Day 10 a"
  r1 <- day10a
  print r1

  putStrLn "Day 10 b"
  r2 <- day10b
  print r2



