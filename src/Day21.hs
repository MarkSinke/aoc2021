module Day21 (day21a, day21b) where

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
  return 0