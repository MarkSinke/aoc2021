module Day25 (day25a, day25b) where

import qualified Data.Matrix as M

data Cell = Empty | East | South deriving (Eq)
type Board = (M.Matrix Cell)

instance Show Cell where
  show Empty = "."
  show East = ">"
  show South = "v"

day25a :: IO Int
day25a = do
  state0 <- readState "/Users/marksinke/IdeaProjects/aoc2021/data/day25input.txt"
  return (runUntilStable 0 state0)

day25b :: IO Int
day25b = do
  return 0

readState :: FilePath -> IO Board
readState path = do
  contents <- readFile path
  let myLines = lines contents
  let cells = map (map readCell) myLines
  return (M.fromLists cells)

readCell :: Char -> Cell
readCell '.' = Empty
readCell '>' = East
readCell 'v' = South
readCell c = error ("bad cel type " ++ [c])

makeMove :: Board -> Board
makeMove board =
  let board1 = M.mapPos (moveEast board (M.ncols board)) board
  in M.mapPos (moveDown board1 (M.nrows board1)) board1

moveEast :: Board -> Int -> (Int, Int) -> Cell -> Cell
moveEast board cols (row, col) Empty =
  if M.getElem row (prevIndex cols col) board == East then East else Empty
moveEast board cols (row, col) East =
  if M.getElem row (nextIndex cols col) board == Empty then Empty else East
moveEast _ _ _ South =
  South

moveDown :: Board -> Int -> (Int, Int) -> Cell -> Cell
moveDown board rows (row, col) Empty =
  if M.getElem (prevIndex rows row) col board == South then South else Empty
moveDown board rows (row, col) South =
  if M.getElem (nextIndex rows row) col board == Empty then Empty else South
moveDown _ _ _ East =
  East

prevIndex :: Int -> Int -> Int
prevIndex range n =
  let newIndex = n - 1
  in if newIndex < 1 then newIndex + range else newIndex

nextIndex :: Int -> Int -> Int
nextIndex range n =
  let newIndex = n + 1
  in if newIndex > range then newIndex - range else newIndex

runUntilStable :: Int -> Board -> Int
runUntilStable steps board =
  let board1 = makeMove board
      steps1 = steps + 1
  in if board == board1 then steps1 else runUntilStable steps1 board1

