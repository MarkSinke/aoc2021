module Day18 (day18a, day18b) where

import Data.Char (digitToInt, isDigit, intToDigit)
import Data.Maybe (fromJust, isJust, mapMaybe)

data SfNumber = Regular Int | Pair SfNumber SfNumber

instance Show SfNumber where
  show = fromSfNumber

day18a :: IO Int
day18a = do
  numbers <- readSnailFishNumbers "/Users/marksinke/IdeaProjects/aoc2021/data/day18input.txt"

--  print (reduce (toSfNumber "[[[[[9,8],1],2],3],4]"))
--  print (reduce (toSfNumber "[7,[6,[5,[4,[3,2]]]]]"))
--  print (reduce (toSfNumber "[[6,[5,[4,[3,2]]]],1]"))
--  print (reduce (toSfNumber "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"))
--  print (reduce (toSfNumber "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"))

  let first = head numbers
  let result = foldl addSfNumber first (tail numbers)

  return (magnitude result)

readSnailFishNumbers :: FilePath -> IO [SfNumber]
readSnailFishNumbers path = do
  contents <- readFile path
  let myLines = lines contents
  return (map toSfNumber myLines)

toSfNumber :: [Char] -> SfNumber
toSfNumber str =
  let (_, num) = toSfNumberRec str
  in num

fromSfNumber :: SfNumber -> String
fromSfNumber (Regular num) =
  [intToDigit num]

fromSfNumber (Pair left right) =
  "[" ++ fromSfNumber left ++ "," ++ fromSfNumber right ++ "]"

toSfNumberRec :: [Char] -> ([Char], SfNumber)
toSfNumberRec (c:cs) | isDigit c = (cs, Regular (digitToInt c))
toSfNumberRec (c:cs) | c == '[' =
    let (r1, left) = toSfNumberRec cs
        r2 = drop 1 r1 -- skip ','
        (r3, right) = toSfNumberRec r2
        r4 = drop 1 r3 -- skip ']'
    in (r4, Pair left right)
toSfNumberRec x = error ("syntax error"  ++ x)

reduce :: SfNumber -> SfNumber
reduce num =
  let (exploded, _, _, explodedNum) = reduceExplode 0 num
  in
    if exploded then reduce explodedNum
    else
      let (split, splitNum) = reduceSplit num
      in
        if split then reduce splitNum
        else splitNum

reduceSplit :: SfNumber -> (Bool, SfNumber)
reduceSplit (Pair left right) =
  let (reducedLeft, newLeft) = reduceSplit left
      (reduced, newRight) = if reducedLeft then (True, right) else reduceSplit right
  in (reduced, Pair newLeft newRight)

reduceSplit (Regular num) =
  if num > 9 then
    let div2 = div num 2
        mod2 = mod num 2
    in (True, Pair (Regular div2) (Regular (div2 + mod2)))
  else (False, Regular num)

reduceExplode :: Int -> SfNumber -> (Bool, Maybe Int, Maybe Int, SfNumber)
reduceExplode depth (Pair (Regular left) (Regular right)) | depth == 4 =
  (True, Just left, Just right, Regular 0)

reduceExplode depth me@(Pair left right) =
  let (reducedLeft, leftAddL, rightAddL, newLeft) = reduceExplode (depth + 1) left
  in
    if reducedLeft then
      if isJust rightAddL then (True, leftAddL, Nothing, Pair newLeft (reduceAddLeft (fromJust rightAddL) right))
      else (True, leftAddL, Nothing, Pair newLeft right)
    else
      let (reducedRight, leftAddR, rightAddR, newRight) = reduceExplode (depth + 1) right
      in
        if reducedRight then
          if isJust leftAddR then (True, Nothing, rightAddR, Pair (reduceAddRight (fromJust leftAddR) left) newRight)
          else (True, Nothing, rightAddR, Pair left newRight)
        else (False, Nothing, Nothing, me)

reduceExplode _ me@(Regular _) = (False, Nothing, Nothing, me)

reduceAddLeft :: Int -> SfNumber -> SfNumber
reduceAddLeft add (Regular num) =
  Regular (num + add)

reduceAddLeft add (Pair left right) =
  Pair (reduceAddLeft add left) right

reduceAddRight :: Int -> SfNumber -> SfNumber
reduceAddRight add (Regular num) =
  Regular (num + add)

reduceAddRight add (Pair left right) =
  Pair left (reduceAddRight add right)

addSfNumber :: SfNumber -> SfNumber -> SfNumber
addSfNumber left right =
  reduce (Pair left right)

magnitude :: SfNumber -> Int
magnitude (Pair left right) =
  3 * magnitude left + 2 * magnitude right

magnitude (Regular num) = num

equals :: SfNumber -> SfNumber -> Bool
equals (Regular left) (Regular right) = left == right
equals (Pair left1 right1) (Pair left2 right2) = equals left1 left2 && equals right1 right2
equals _ _ = False

day18b :: IO Int
day18b = do
  numbers <- readSnailFishNumbers "/Users/marksinke/IdeaProjects/aoc2021/data/day18input.txt"

  let combinations = concatMap (\num -> mapMaybe (numPair num) numbers) numbers
  return (maximum (map magSumPair combinations))

magSumPair :: (SfNumber, SfNumber) -> Int
magSumPair (a, b) = magnitude (addSfNumber a b)

numPair :: SfNumber -> SfNumber -> Maybe (SfNumber, SfNumber)
numPair left right =
  if equals left right then Nothing else Just (left, right)
