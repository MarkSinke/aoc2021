module Day22 (day22a, day22b, ifoldr3D, toList) where

import Text.Regex
import Common
import Data.Maybe (fromJust)
import Data.List.Index

data Point = Point Int Int Int deriving (Show)
data Instruction = Instruction Bool (Int, Int, Int) (Int, Int, Int) deriving (Show)
data Cube = Cube Int [Bool]

toList :: Cube -> [Bool]
toList (Cube _ xs) = xs

createCube :: Int -> Cube
createCube maxVal =
  let size = maxVal * 2 + 1
  in Cube maxVal (replicate (size * size * size) False)

day22a :: IO Int
day22a = do
  instructions <- readInstructions "/Users/marksinke/IdeaProjects/aoc2021/data/day22input.txt"

  let maxVal = 50
  let cube = createCube maxVal
  let finalCube = foldl doInstruction cube instructions
  return (countOn finalCube)

countOn :: Cube -> Int
countOn cube = foldr3D (\x acc -> acc + toInt x) 0 cube

toInt :: Bool -> Int
toInt True = 1
toInt False = 0

doInstruction :: Cube -> Instruction -> Cube
doInstruction input (Instruction False p0 p1) =
  if not (within (-50, -50, -50) (50, 50, 50) p0) then input
  else imap3D (\x b -> b && (not (within p0 p1 x))) input
doInstruction input (Instruction True p0 p1) =
  if not (within (-50, -50, -50) (50, 50, 50) p0) then input
  else imap3D (\x b -> b || within p0 p1 x) input

within :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> Bool
within (x0, y0, z0) (x1, y1, z1) (x, y, z) =
  x0 <= x && x <= x1 &&
  y0 <= y && y <= y1 &&
  z0 <= z && z <= z1

foldr3D :: (Bool -> acc -> acc) -> acc -> Cube -> acc
foldr3D f z (Cube _ cube) =
  foldr f z cube

ifoldr3D :: ((Int, Int, Int) -> Bool -> acc -> acc) -> acc -> Cube -> acc
ifoldr3D f zero (Cube maxVal cube) =
  ifoldr (\i b acc -> f (toPos maxVal i) b acc) zero cube

imap3D :: ((Int, Int, Int) -> Bool -> Bool) -> Cube -> Cube
imap3D f (Cube maxVal cube) =
  Cube maxVal (imap (\i x -> f (toPos maxVal i) x) cube)

toPos :: Int -> Int -> (Int, Int, Int)
toPos maxVal i =
  let size = maxVal * 2 + 1
      z = div i (size * size)
      r1 = mod i (size * size)
      y = div r1 size
      x = mod r1 size
      pos = (x - maxVal, y - maxVal, z - maxVal)
  in pos

day22b :: IO Int
day22b = do
  return 0

readInstructions :: FilePath -> IO [Instruction]
readInstructions path = do
  contents <- readFile path
  let myLines = lines contents
  return (map toInstruction myLines)

toInstruction :: String -> Instruction
toInstruction str =
  let intPattern = "-?[0-9]+"
      rangePattern = "(" ++ intPattern ++ ")\\.\\.(" ++ intPattern ++ ")"
      pattern = mkRegex ("(on|off) x=" ++ rangePattern ++ ",y=" ++ rangePattern ++ ",z=" ++ rangePattern)
      tokens = fromJust (matchRegex pattern str)
      on = toOnOff (head tokens)
  in case map readInt (tail tokens) of
        [x0, x1, y0, y1, z0, z1] -> Instruction on (x0, y0, z0) (x1, y1, z1)
        _ -> error ("missing tokens: " ++ show tokens)

toOnOff :: String -> Bool
toOnOff "on" = True
toOnOff "off" = False
toOnOff str = error ("not on or off: " ++ str)
