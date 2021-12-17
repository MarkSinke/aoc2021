module Day9 (day9a, day9b)
where

import Data.List (transpose, sort)
import Data.Maybe
import qualified Data.Matrix as Matrix

day9a :: IO Int
day9a = do
  heights <- readHeightMap "/Users/marksinke/IdeaProjects/aoc2021/data/day9input.txt"
  let profile = getRiskProfile heights
  let score = foldl (foldl (+)) 0 profile
  return score

day9b :: IO Int
day9b = do
  heights <- readHeightMap "/Users/marksinke/IdeaProjects/aoc2021/data/day9input.txt"
  let riskProfile = getRiskProfile heights
  let heightMatrix = Matrix.fromLists heights
  let riskMatrix = Matrix.fromLists riskProfile
  let coordsMatrix = Matrix.mapPos toRiskCoord riskMatrix
  let coords = catMaybes (Matrix.toList coordsMatrix)
  let coloredMaps = map (colorMap heightMatrix (-1)) coords
  let sizes = map getColoredSize coloredMaps
  let sortedSizes = reverse (sort sizes)
  return (product (take 3 sortedSizes))

getColoredSize :: Matrix.Matrix Int -> Int
getColoredSize matrix =
  let list = Matrix.toList matrix
  in foldl flattenColor 0 list

flattenColor :: Int -> Int -> Int
flattenColor a x = a + if x == -1 then 1 else 0

allDirections :: [(Int, Int)]
allDirections = [(-1, 0), (0, -1), (1, 0), (0, 1)]

colorMap :: Matrix.Matrix Int -> Int -> (Int, Int) -> Matrix.Matrix Int
colorMap matrix num coord =
  colorMapRec num matrix [coord]

colorMapRec :: Int -> Matrix.Matrix Int -> [(Int, Int)] -> Matrix.Matrix Int
colorMapRec num matrix steps =
  if null steps then matrix
  else let step = head steps
           newMatrix = Matrix.setElem num step matrix
       in colorMapRec num newMatrix (tail steps ++ validSteps num newMatrix step)

validSteps :: Int -> Matrix.Matrix Int -> (Int, Int) -> [(Int, Int)]
validSteps num matrix coord =
  filter (isValidCell num matrix) (map (doStep coord) allDirections)

doStep :: (Int, Int) -> (Int, Int) -> (Int, Int)
doStep (x, y) (dx, dy) = (x + dx, y + dy)

isValidCell :: Int -> Matrix.Matrix Int -> (Int, Int) ->Bool
isValidCell num matrix (r1, c1) =
  not (c1 == 0 ||  c1 > Matrix.ncols matrix || r1 == 0 || r1 > Matrix.nrows matrix || el == 9 || el == num)
  where el = Matrix.getElem r1 c1 matrix

toRiskCoord :: (Int, Int) -> Int -> Maybe (Int, Int)
toRiskCoord (r, c) val =
  if val /= 0 then Just (r, c) else Nothing

readHeightMap :: FilePath -> IO [[Int]]
readHeightMap path = do
  contents <- readFile path
  let myLines = lines contents
  let heights = map readHeightLine myLines
  return heights

getRiskProfile :: [[Int]] -> [[Int]]
getRiskProfile heights =
  let profileH = map createRiskLine heights
      profileV = transpose (map createRiskLine (transpose heights))
      profile = zipWith (zipWith getRisk) profileH profileV
  in profile

getRisk :: Int -> Int -> Int
getRisk a b = min a b + 1

createRiskLine :: [Int] -> [Int]
createRiskLine = createRiskRec 10

createRiskRec :: Int -> [Int] -> [Int]
createRiskRec prev row =
  let h = head row
      t = tail row
  in if null t then [if prev > h then h else -1]
  else getMin prev h (head t) : createRiskRec h t



getMin :: Int -> Int -> Int -> Int
getMin x0 x1 x2 = if x0 > x1 && x1 < x2 then x1 else -1

readHeightLine :: String -> [Int]
readHeightLine =
  map readCharInt

readCharInt :: Char -> Int
readCharInt ch = read (ch : "")