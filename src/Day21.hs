module Day21 (day21a, day21b) where

import Data.List (group, sort)


--data GameState = GameState {turns :: Int, player1Score :: Int, player1Pos :: Int, player2Score :: Int, player2Pos :: Int, whoseTurn :: Int}
data GameState = GameState Int Int Int Int Int Int deriving (Show)

createInitialState :: Int -> Int -> GameState
createInitialState p1 p2 = GameState 0 0 p1 0 p2 1

day21a :: IO Int
day21a = do
  let state = createInitialState 9 3
--  let state = createInitialState 4 8
  let die = cycle [1..100]

  let (GameState turns p1score _ p2score _ whose, _) = runGameUntil 1000 state die

  return ((if whose == 1 then p1score else p2score) * turns * 3)

runGameUntil :: Int -> GameState -> [Int] -> (GameState, [Int])
runGameUntil limit state@(GameState _ p1s _ p2s _ _) die =
  if p1s >= limit || p2s >= limit then (state, die)
  else
    let (nextS, nextDie) = nextState (state, die)
    in runGameUntil limit nextS nextDie

nextState :: (GameState, [Int]) -> (GameState, [Int])
nextState (state, die) =
  let throws = sum (take 3 die)
      nextDie = drop 3 die
  in (advance state throws, nextDie)

advance :: GameState -> Int -> GameState
advance (GameState turns player1Score player1Pos player2Score player2Pos whoseTurn) dist =
  if whoseTurn == 1 then
    let nextPos = mod (player1Pos + dist - 1) 10 + 1
    in GameState (turns + 1) (player1Score + nextPos) nextPos player2Score player2Pos 2
  else
    let nextPos = mod (player2Pos + dist - 1) 10 + 1
    in GameState (turns + 1) player1Score player1Pos (player2Score + nextPos) nextPos 1

day21b :: IO Int
day21b = do
  let state = createInitialState 9 3
--  let state = createInitialState 4 8

  let result = countWinsUntil 21 1 state
  let m = uncurry max result
  return m

countWinsUntil :: Int -> Int -> GameState -> (Int, Int)
countWinsUntil limit factor state@(GameState _ p1s _ p2s _ _)
  | p1s >= limit = (factor, 0)
  | p2s >= limit = (0, factor)
  | otherwise = 
    let results = map (nextCounts limit factor state) distFactors
    in (sum (map fst results), sum (map snd results))

nextCounts :: Int -> Int -> GameState -> (Int, Int) -> (Int, Int)
nextCounts limit factor state (dist, count) =
  let next = advance state dist
  in countWinsUntil limit (factor * count) next

-- list of sums of die throws and how often they occur
distFactors :: [(Int, Int)]
distFactors =
  let allSums = concatMap (\a -> concatMap (\b -> map (\c -> a + b + c) [1..3]) [1..3]) [1..3]
  in map (\x -> (head x, length x)) (group (sort allSums))

