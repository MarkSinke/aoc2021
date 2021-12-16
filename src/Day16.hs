module Day16 (day16a, day16b )where

import Data.Char (digitToInt)
import Text.Printf (printf)
import Data.Maybe (fromJust)

data Packet = Packet { version :: Int, typeId :: Int, literal :: Maybe Int, subPackets :: [Packet] } deriving (Show)

day16a :: IO Int
day16a = do
  bits <- readBits "/Users/marksinke/IdeaProjects/aoc2021/data/day16input.txt"
  let (_, packet) = toPacket bits
  return (sumVersions packet)

day16b :: IO Int
day16b = do
  bits <- readBits "/Users/marksinke/IdeaProjects/aoc2021/data/day16input.txt"
  let (_, packet) = toPacket bits
  return (evaluatePacket packet)

evaluatePacket :: Packet -> Int
evaluatePacket (Packet _ 0 _ sub) = sum (map evaluatePacket sub)
evaluatePacket (Packet _ 1 _ sub) = product (map evaluatePacket sub)
evaluatePacket (Packet _ 2 _ sub) = minimum (map evaluatePacket sub)
evaluatePacket (Packet _ 3 _ sub) = maximum (map evaluatePacket sub)
evaluatePacket (Packet _ 4 lit _) = fromJust lit
evaluatePacket (Packet _ 5 _ sub) = evalCompare (>) sub
evaluatePacket (Packet _ 6 _ sub) = evalCompare (<) sub
evaluatePacket (Packet _ 7 _ sub) = evalCompare (==) sub
evaluatePacket _ = error "bad packet"

evalCompare :: (Int -> Int -> Bool) -> [Packet] -> Int
evalCompare f [a, b] =
  if f (evaluatePacket a) (evaluatePacket b) then 1 else 0

evalCompare _ _ =
  error "packet structure for comparison"

sumVersions :: Packet -> Int
sumVersions (Packet v _ _ sub) =
  v + sum (map sumVersions sub)

toPacket :: [Char] -> ([Char], Packet)
toPacket bits =
  let (r1, v) = takeBits 3 bits
      (r2, t) = takeBits 3 r1
  in if t == 4 then
    let (r3, lit) = takeLiteral r2
    in (r3, Packet v t (Just lit) [])
  else let (r3, sub) = toSubPackets r2
    in (r3, Packet v t Nothing sub)

toSubPackets :: [Char] -> ([Char], [Packet])
toSubPackets str =
  let (r1, lengthTypeId) = takeBits 1 str
  in if lengthTypeId == 0
    then
      let (r2, bitCount) = takeBits 15 r1
          (_, packets) = toNPackets (-1) [] (take bitCount r2)
      in (drop bitCount r2, packets)
    else
      let (r2, count) = takeBits 11 r1
      in toNPackets count [] r2

toNPackets :: Int -> [Packet] -> [Char] -> ([Char], [Packet])
toNPackets num acc str =
  if num == 0 || null str then (str, acc) else
    let (r1, packet) = toPacket str
    in toNPackets (num -1) (acc ++ [packet]) r1

takeLiteral :: [Char] -> ([Char], Int)
takeLiteral = takeLiteralRec 0

takeLiteralRec :: Int -> [Char] -> ([Char], Int)
takeLiteralRec n str =
  let (r1, prefix) = takeBits 1 str
      (r2, value) = takeBits 4 r1
      newValue = n * 16 + value
  in if prefix == 0 then (r2, newValue) else takeLiteralRec newValue r2

takeBits :: Int -> [Char] -> ([Char], Int)
takeBits n str =
  let value = fromBits (take n str)
      remainder = drop n str
  in (remainder, value)

fromBits :: [Char] -> Int
fromBits = foldl (\n x -> 2 * n + digitToInt x) 0

readBits :: FilePath -> IO String
readBits path = do
  contents <- readFile path
  return (concatMap toBinaryDigits contents)

toBinaryDigits :: Char -> [Char]
toBinaryDigits c =
  let x = digitToInt c
  in printf "%04b" (x::Int)