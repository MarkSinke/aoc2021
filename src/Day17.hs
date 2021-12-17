module Day17 (day17a, day17b) where

import Data.Maybe (mapMaybe)

-- no need to read file. All it has is
-- target area: x=144..178, y=-100..-76

day17a :: IO Int
day17a = do
  let result = concatMap scanY [0..1000]
      heights = mapMaybe toHeight result
      m = foldl maxHeight ((0, 0), 0) heights
  return (snd m)

day17b :: IO Int
day17b = do
    let result = concatMap scanY [-100..1000]
        hits = filter isHit result
    return (length hits)

isHit :: (Bool, (Int, Int), [(Int, Int)]) -> Bool
isHit (ok, _, _) = ok

maxHeight :: ((Int, Int), Int) -> ((Int, Int), Int) -> ((Int, Int), Int)
maxHeight acc result =
  if snd result > snd acc then result else acc

toHeight :: (Bool, (Int, Int), [(Int, Int)]) -> Maybe ((Int, Int), Int)
toHeight (ok, dir, path) =
  if not ok then Nothing
  else Just (dir, maximum (map snd path))

scanY :: Int -> [(Bool, (Int, Int), [(Int, Int)])]
scanY dy =
  map (scanX dy) [1..200]

scanX :: Int -> Int -> (Bool, (Int, Int), [(Int, Int)])
scanX dy dx =
  let (ok, path) = trajectory [] (dx, dy) (0, 0)
  in (ok, (dx, dy), path)

trajectory :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> (Bool, [(Int, Int)])
trajectory acc dir pos =
  let newAcc = pos : acc
  in if isOffTrack pos then (False, reverse newAcc)
  else if isWithinTarget pos then (True, reverse newAcc)
  else trajectory newAcc (nextDir dir) (plus pos dir)

plus :: (Int, Int) -> (Int, Int) -> (Int, Int)
plus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

isWithinTarget :: (Int, Int) -> Bool
isWithinTarget (x, y) =
  x >= 144 && x <= 178 && y >= -100 && y <= -76

nextDir :: (Int, Int) -> (Int, Int)
nextDir (dx, dy) = (dx - signum dx, dy - 1)

isOffTrack :: (Int, Int) -> Bool
isOffTrack (_, y) =
  y < -100
