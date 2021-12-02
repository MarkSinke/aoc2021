module Main where

import Lib (day1a, day1b, day2a, day2b)

main :: IO ()
main = do
  print "Day 2 a"
  r1 <- day2a
  print r1
  r2 <- day2b
  print "Day 2 b"
  print r2
  print (case r2 of (hor, depth, aim) -> hor * depth)