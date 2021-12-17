module Main where

import Lib (day1a, day1b, day2a, day2b, day3a, day3b, day4a, day4b, day5a, day5b, day6a, day6b)
import Day7 (day7a, day7b)
import Day8 (day8a, day8b)
import Day9 (day9a, day9b)
import Day10 (day10a, day10b)
import Day11 (day11a, day11b)
import Day12 (day12a, day12b)
import Day13 (day13a, day13b)
import Day14 (day14a, day14b)
import Day15 (day15a, day15b)
import Day16 (day16a, day16b)
import Day17 (day17a, day17b)

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
--  printDay10
--  printDay11
--  printDay12
--  printDay13
--  printDay14
--  printDay15
--  printDay16
  printDay17

printDay1 :: IO ()
printDay1 = printDay "Day 1" day1a day1b

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

printDay :: (Show a, Show b) => String -> IO a -> IO b -> IO ()
printDay name fa fb = do
  putStrLn (name ++ "a")
  r1 <- fa
  print r1

  putStrLn (name ++ "b")
  r2 <- fb
  print r2

printDay5 :: IO ()
printDay5 = printDay "Day 5" day5a day5b
printDay6 :: IO ()
printDay6 = printDay "Day 6" day6a day6b
printDay7 :: IO ()
printDay7 = printDay "Day 7" day7a day7b
printDay8 :: IO ()
printDay8 = printDay "Day 8" day8a day8b
printDay9 :: IO ()
printDay9 = printDay "Day 9" day9a day9b
printDay10 :: IO ()
printDay10 = printDay "Day 10" day10a day10b
printDay11 :: IO ()
printDay11 = printDay "Day 11" day11a day11b
printDay12 :: IO ()
printDay12 = printDay "Day 12" day12a day12b
printDay13 :: IO ()
printDay13 = printDay "Day 13" day13a day13b
printDay14 :: IO ()
printDay14 = printDay "Day 14" day14a day14b
printDay15 :: IO ()
printDay15 = printDay "Day 15" day15a day15b
printDay16 :: IO ()
printDay16 = printDay "Day 16" day16a day16b
printDay17 :: IO ()
printDay17 = printDay "Day 17" day17a day17b


