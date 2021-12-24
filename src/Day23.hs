module Day23 (day23a, day23b) where

import Debug.Trace
import Data.Foldable (find)
import Data.Maybe (isJust, fromJust)

data Amphipod = Amber | Bronze | Copper| Desert deriving (Show)

moveCost :: Amphipod -> Int
moveCost Amber = 1
moveCost Bronze = 10
moveCost Copper = 100
moveCost Desert = 1000

data NodeType = Free | Move | Room Amphipod
data Node = Node NodeType Int Int Int Int -- type left up right down, indexed by int
newtype Board = Board [Node]

none :: Int
none = -1

createBoard :: Board
createBoard = Board
  [
  --        L    U    R    D
  Node Free none none    1 none, -- 0
  Node Free    0 none    2 none, -- 1
  Node Move    1 none    3   11, -- 2
  Node Free    2 none    4 none, -- 3
  Node Move    3 none    5   13, -- 4
  Node Free    4 none    6 none, -- 5
  Node Move    5 none    7   15, -- 6
  Node Free    6 none    8 none, -- 7
  Node Move    7 none    9   17, -- 8
  Node Free    8 none   10 none, -- 9
  Node Free    9 none none none, -- 10

  Node (Room Amber)  none    2 none   12, -- 11
  Node (Room Amber)  none   11 none none, -- 12
  Node (Room Bronze) none    4 none   14, -- 13
  Node (Room Bronze) none   13 none none, -- 14
  Node (Room Copper) none    6 none   16, -- 15
  Node (Room Copper) none   15 none none, -- 16
  Node (Room Desert) none    8 none   18, -- 17
  Node (Room Desert) none   17 none none  -- 18
  ]

isFinal :: Board -> State -> Bool
isFinal board (State _ podLocations) =
   all (isFinalRoom board) podLocations

isFinalRoom :: Board -> (Amphipod, Int) -> Bool
isFinalRoom _ (Amber, n) = n == 11 || n == 12
isFinalRoom _ (Bronze, n) = n == 13 || n == 14
isFinalRoom _ (Copper, n) = n == 15 || n == 16
isFinalRoom _ (Desert, n) = n == 17 || n == 18

nextNodeIndices :: Board -> Amphipod -> Int -> [Int]
nextNodeIndices board@(Board nodes) pod index =
  let (Node _ left up right down) = nodes !! index
  in if isFinalRoom board (pod, index) && even index then []
     else
        let actualDown = if down /= none && isFinalRoom board (pod, down) then down else none
        in filter (/= none) [left, up, right, actualDown]

data State = State Int [(Amphipod, Int)] deriving (Show) -- total energy spent, type and position of each amphipod

initialState :: State
initialState = State 0 [(Amber, 14), (Amber, 17), (Bronze, 11), (Bronze, 13),
  (Copper, 12), (Copper, 18), (Desert, 15), (Desert, 16)]

isNodeUnoccupied :: State -> Int -> Bool
isNodeUnoccupied (State _ pods) index =
  all (\(_, location) -> location /= index) pods

movePod :: State -> Amphipod -> Int -> Int -> State
movePod (State energy pods) podType podIndex nodeIndex =
--  trace ("move " ++ show podIndex ++ " to " ++ show nodeIndex)
  State (energy + moveCost podType) (setAt pods podIndex (podType, nodeIndex))

setAt :: [a] -> Int -> a -> [a]
setAt xs n x =
  let (left, right) = splitAt n xs
  in left ++ [x] ++ tail right

nextStatesForPod :: Board -> State -> Int -> [State]
nextStatesForPod board state@(State _ pods) podIndex =
  let (podType, location) = pods !! podIndex
      nodeIndices = filter (isNodeUnoccupied state) (nextNodeIndices board podType location)
  in map (movePod state podType podIndex) nodeIndices

nextStates :: Board -> State -> [State]
nextStates board state =
  concatMap (nextStatesForPod board state) [0..7]

energy :: State -> Int
energy (State energy _) = energy

computeMoves :: Board -> [State] -> State
computeMoves board (state : states) =
  if isFinal board state then state
  else
    let addStates = if energy state > 12000 then [] else nextStates board state
    in -- trace ("states added " ++ concatMap printState addStates)
    computeMoves board (states ++ addStates)

computeMoves _ [] =
  error "out of options"

printState :: State -> String
printState state@(State energy pods) =
  map (printCell state) [0..10] ++ " (energy: " ++ show energy ++ ")\n" ++
  "  " ++ concatMap(\i -> [printCell state i, ' ']) [11,13,15,17] ++ "\n" ++
  "  " ++ concatMap(\i -> [printCell state i, ' ']) [12,14,16,18] ++ "\n"

printCell :: State -> Int -> Char
printCell (State energy pods) i =
  let result = find (\(podType, location) -> i == location) pods
  in
    if isJust result then
      let (podType, _) = fromJust result
      in podLetter podType
    else '.'

podLetter Amber = 'A'
podLetter Bronze = 'B'
podLetter Copper = 'C'
podLetter Desert = 'D'

day23a :: IO Int
day23a = do
  let board = createBoard
  let state0 = initialState
  let (State energy _) = computeMoves board [state0]
  return energy

day23b :: IO Int
day23b = do
  return 0
