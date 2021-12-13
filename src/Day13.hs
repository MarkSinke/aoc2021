module Day13 (day13a, day13b) where

import Common
import qualified Data.Matrix as Matrix
import Data.List (transpose)

data Fold = Fold { isX :: Bool, line :: Int } deriving (Show)

day13a :: IO Int
day13a = do
  (grid, folds) <- readGrid "/Users/marksinke/IdeaProjects/aoc2021/data/day13input.txt"
  let grid1 = doFold grid (head folds)
  return (countDots grid1)

day13b :: IO String
day13b = do
  (grid, folds) <- readGrid "/Users/marksinke/IdeaProjects/aoc2021/data/day13input.txt"
  let finalGrid = foldl doFold grid folds
  putStrLn (toDots finalGrid)
  return (toDots finalGrid)

toDots :: [[Bool]] -> String
toDots = concatMap toDotRow

toDotRow :: [Bool] -> String
toDotRow xs = map (\x -> if x then '#' else '.') xs ++ "\n"

countDots :: [[Bool]] -> Int
countDots grid = length (filter id (concat grid))

doFold :: [[Bool]] -> Fold -> [[Bool]]
doFold input fold =
  if isX fold then doFoldAlongVertical (line fold) input else doFoldAlongHorizontal (line fold) input

doFoldAlongHorizontal :: Int -> [[Bool]] -> [[Bool]]
doFoldAlongHorizontal y input =
  let rows = length input
      cols = length (head input)
      resultLength = max y (rows - y - 1)
      emptyRow = replicate cols False

      -- fold first half over second half ("down")
      firstHalf = take resultLength (reverse (take y input) ++ repeat emptyRow)
      secondHalf = take resultLength (drop (y + 1) input ++ repeat emptyRow)

      -- and then reverse the result
      result = reverse (zipWith (zipWith (||)) firstHalf secondHalf)
   in result

doFoldAlongVertical :: Int -> [[Bool]] -> [[Bool]]
doFoldAlongVertical x input = transpose (doFoldAlongHorizontal x (transpose input))

readGrid :: FilePath -> IO ([[Bool]], [Fold])
readGrid path = do
  contents <- readFile path
  let myLines = lines contents
  let (gridLines, foldLines) = break null myLines
  let grid = makeGrid (map toCoord gridLines)
  let folds = map toFold (tail foldLines)
  return (grid, folds)

makeGrid :: [(Int, Int)] -> [[Bool]]
makeGrid coords =
  let maxX = maximum (map fst coords) + 1
      maxY = maximum (map snd coords) + 1
      mat = Matrix.matrix maxY maxX (const False)
  in Matrix.toLists (foldl insertCoord mat coords)

insertCoord :: Matrix.Matrix Bool -> (Int, Int) -> Matrix.Matrix Bool
insertCoord input (x, y) =
  Matrix.setElem True (y + 1, x + 1) input

toFold :: String -> Fold
toFold str =
  let w = words str
      instruction = (head . tail . tail) w
      instructionParts = split '=' instruction
      x = head instructionParts == "x"
      val = readInt (instructionParts !! 1)
  in Fold x val
