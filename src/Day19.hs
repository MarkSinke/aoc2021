module Day19 (day19a, day19b) where

import Data.List.Split (splitWhen)
import Common
import Data.List (sort, group, union)
import Data.Maybe (fromJust, isJust)

data Coord = Coord Int Int Int deriving (Eq, Ord)
data Matrix = Matrix Int Int Int Int Int Int Int Int Int deriving (Show)

instance Show Coord where
  show (Coord x y z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

day19a :: IO Int
day19a = do
  scannerCoords <- readScannerCoords "/Users/marksinke/IdeaProjects/aoc2021/data/day19input.txt"

  let result = length (findCoords (head scannerCoords) (tail scannerCoords))
  return result

findCoords :: [Coord] -> [[Coord]] -> [Coord]
findCoords acc (coords:scanners) =
  let result = matchAny acc coords transforms
  in
    if isJust result then
      let (transform, dist) = fromJust result
      in findCoords (combineCoords acc transform dist coords) scanners
    else findCoords acc (scanners ++ [coords])

findCoords acc [] = acc

combineCoords :: [Coord] -> Matrix -> Coord -> [Coord] -> [Coord]
combineCoords acc transform dist coords =
  let newCoords = map (\x -> minus (multiply transform x) dist) coords
  in union acc newCoords

matchAny :: [Coord] -> [Coord] -> [Matrix] -> Maybe (Matrix, Coord)
matchAny scanner0 scanner1 (m:ms) =
  let result = match scanner0 scanner1 m
  in
    if isJust result then Just (m, fromJust result)
    else matchAny scanner0 scanner1 ms

matchAny _ _ [] = Nothing

match :: [Coord] -> [Coord] -> Matrix -> Maybe Coord
match scanner0 scanner1 transform1 =
  let dists = concatMap (computeDists transform1 scanner1) scanner0
      sorted = sort dists
      grouped = group sorted
      found = filter (\x -> length x >= 12) grouped
  in if null found then Nothing else Just (head (head found))

computeDists :: Matrix -> [Coord] -> Coord -> [Coord]
computeDists transform coords coord =
  map (\x -> minus (multiply transform x) coord) coords

minus :: Coord -> Coord -> Coord
minus (Coord x1 y1 z1) (Coord x2 y2 z2) =
  Coord (x1 - x2) (y1 - y2) (z1 - z2)

day19b :: IO Int
day19b = do
  return 0

readScannerCoords :: FilePath -> IO [[Coord]]
readScannerCoords path = do
  contents <- readFile path
  let myLines = lines contents
  let scannerGroups = splitWhen null myLines
  return (map toCoordsList scannerGroups)

toCoordsList :: [String] -> [Coord]
toCoordsList xs =
  map toCoords (tail xs)

toCoords :: String -> Coord
toCoords str =
  let parts = split ',' str
  in Coord (readInt (head parts)) (readInt (parts !! 1)) (readInt (parts !! 2))

multiplyM :: Matrix -> Matrix -> Matrix
multiplyM (Matrix a11 a12 a13 a21 a22 a23 a31 a32 a33) (Matrix b11 b12 b13 b21 b22 b23 b31 b32 b33) =
  Matrix (a11 * b11 + a12 * b21 + a13 * b31)
         (a11 * b12 + a12 * b22 + a13 * b32)
         (a11 * b13 + a12 * b23 + a13 * b33)
         (a21 * b11 + a22 * b21 + a23 * b31)
         (a21 * b12 + a22 * b22 + a23 * b32)
         (a21 * b13 + a22 * b23 + a23 * b33)
         (a31 * b11 + a32 * b21 + a33 * b31)
         (a31 * b12 + a32 * b22 + a33 * b32)
         (a31 * b13 + a32 * b23 + a33 * b33)

multiply :: Matrix -> Coord -> Coord
multiply (Matrix a11 a12 a13 a21 a22 a23 a31 a32 a33) (Coord x y z) =
  Coord (a11 * x + a12 * y + a13 * z) (a21 * x + a22 * y + a23 * z) (a31 * x + a32 * y + a33 * z)

unity :: Matrix
unity = Matrix 1    0    0        0    1    0        0    0    1

rotX90 :: Matrix
rotX90 = Matrix 1    0    0        0    0  (-1)       0    1    0
rotX180 :: Matrix
rotX180 = multiplyM rotX90 rotX90
rotX270 :: Matrix
rotX270 = multiplyM rotX180 rotX90

rotY90 :: Matrix
rotY90 = Matrix 0    0    1        0    1    0      (-1)   0    0
rotY180 :: Matrix
rotY180 = multiplyM rotY90 rotY90
rotY270 :: Matrix
rotY270 = multiplyM rotY180 rotY90

rotZ90 :: Matrix
rotZ90 = Matrix 0  (-1)   0        1    0    0        0    0    1
rotZ180 :: Matrix
rotZ180 = multiplyM rotZ90 rotZ90
rotZ270 :: Matrix
rotZ270 = multiplyM rotZ180 rotZ90

axes :: [Matrix]
axes = [ unity, rotY90, rotY180, rotY270, rotZ90, rotZ270 ]

transforms :: [Matrix]
transforms = concatMap (\m -> map (multiplyM m) [ unity, rotX90, rotX180, rotX270]) axes
