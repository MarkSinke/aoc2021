module Day22 (day22a, day22b) where

import Text.Regex
import Common
import Data.Maybe (fromJust)
import Data.List (sort, foldl')
import Debug.Trace

data Point = Point Int Int Int deriving (Show)
data Instruction = Instruction Bool (Int, Int, Int) (Int, Int, Int) deriving (Show)
data LitRegion = LitRegion (Int, Int, Int) (Int, Int, Int) deriving (Show)
data LitCube = LitCube [LitRegion] deriving (Show)

day22a :: IO Int
day22a = do
  instructions <- readInstructions "/Users/marksinke/IdeaProjects/aoc2021/data/day22input.txt"
  let newInstr = filter (\(Instruction _ p0 _) -> (within (-50, -50, -50) (50, 50, 50) p0)) instructions

  let finalCube = foldl doLitInstruction (LitCube []) newInstr

  return (countLitOn finalCube)

within :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> Bool
within (x0, y0, z0) (x1, y1, z1) (x, y, z) =
  x0 <= x && x <= x1 &&
  y0 <= y && y <= y1 &&
  z0 <= z && z <= z1

outside :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> Bool
outside (x0, y0, z0) (x1, y1, z1) (x, y, z) =
  x < x0 || x1 < x ||
  y < y0 || y1 < y ||
  z < z0 || z1 < z


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

day22b :: IO Int
day22b = do
  instructions <- readInstructions "/Users/marksinke/IdeaProjects/aoc2021/data/day22input.txt"

  let finalCube = foldl' doLitInstruction (LitCube []) instructions

  return (countLitOn finalCube)

countLitOn :: LitCube -> Int
countLitOn (LitCube regions) =
  sum (map size regions)

countRegions :: LitCube -> Int
countRegions (LitCube regions) =
  length regions

size :: LitRegion -> Int
size (LitRegion (x0, y0, z0) (x1, y1, z1)) =
  (x1 - x0 + 1) * (y1 - y0 + 1) * (z1 - z0 + 1)

doLitInstruction :: LitCube -> Instruction -> LitCube
doLitInstruction (LitCube regions) i@(Instruction True p0 p1) =
  let onRegion = LitRegion p0 p1
      result = LitCube (onRegion : (concatMap (turnOffSection onRegion) regions))
  in trace (show i ++ ">" ++ show (countRegions result)) result

doLitInstruction (LitCube regions) i@(Instruction False p0 p1) =
  let offRegion = LitRegion p0 p1
      result = LitCube (concatMap (turnOffSection offRegion) regions)
  in trace (show i ++ ">" ++ show (countRegions result)) result

turnOffSection :: LitRegion -> LitRegion -> [LitRegion]
turnOffSection offRegion@(LitRegion (xoff0, yoff0, zoff0) (xoff1, yoff1, zoff1)) (LitRegion (xon0, yon0, zon0) (xon1, yon1, zon1)) =
  let xRanges = ranges xon0 xon1 xoff0 xoff1
      yRanges = ranges yon0 yon1 yoff0 yoff1
      zRanges = ranges zon0 zon1 zoff0 zoff1
      regions = [LitRegion (x0, y0, z0) (x1, y1, z1) | (x0, x1) <- xRanges, (y0, y1) <- yRanges, (z0, z1) <- zRanges]
      litRegions = (filter (regionOutside offRegion) regions)
   in litRegions

ranges :: Int -> Int -> Int -> Int ->  [(Int, Int)]
ranges on0 on1 off0 off1 =
  let [p0, p1, p2, p3] = sort (map (bound on0 (on1 + 1)) [on0, on1 + 1, off0, off1 + 1])
      intervals = filter (\(a, b) -> a <= b) [(p0, p1 - 1), (p1, p2 - 1), (p2, p3 - 1)]
  in intervals

bound :: Int -> Int -> Int -> Int
bound a b x = min b (max a x)

regionOutside :: LitRegion -> LitRegion -> Bool
regionOutside (LitRegion outer0 outer1) (LitRegion inner0 inner1) =
  outside outer0 outer1 inner0 || outside outer0 outer1 inner1