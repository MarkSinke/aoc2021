module Main where

import Lib (day1a, day1b)

main :: IO ()
main = do
  r1 <- day1a
  print r1
  r2 <- day1b
  print r2