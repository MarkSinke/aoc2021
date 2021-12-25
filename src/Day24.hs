module Day24 (day24a, day24b) where

import Common
import Data.Char (digitToInt)
import Data.List (foldl', find)
import Debug.Trace

day24a :: IO Int
day24a = do
  accu <- readOps "/Users/marksinke/IdeaProjects/aoc2021/data/day24input.txt"
  let z = getRegister accu "z"
  let nums = [99999999999999, 99999999999998..11111111111111]
  let result = z `seq` find (isZero z) nums
  print result
  return (getPos accu)

isZero :: Expression -> Int -> Bool
isZero z n =
  let str = show n :: [Char]
  in (if mod n 10000 == 0 then trace (show n) else id)
    ('0' `notElem` str &&  exec z (show n) == 0)

day24b :: IO Int
day24b = do
  return 0

data Expression =
  Literal Int |
  Inp Int |
  Add Expression Expression |
  Mul Expression Expression |
  Div Expression Expression |
  Mod Expression Expression |
  Eql Expression Expression
  deriving (Show)

data Accu = Accu Int Expression Expression Expression Expression deriving (Show)-- inputPos w x y z

range :: Expression -> (Int, Int)
range (Literal n) = (n, n)
range (Inp _) = (1, 9)
range (Add e1 e2) =
  let (from1, to1) = range e1
      (from2, to2) = range e2
  in (from1 + from2, to1 + to2)
range (Mul e1 e2) =
  let (from1, to1) = range e1
      (from2, to2) = range e2
  in
    if from1 < 0 || to1 < 0 || from2 < 0 || to2 < 0
    then error ("bad range " ++ show (from1, to1, from2, to2))
    else (from1 * from2, to1 * to2)
range (Div e1 e2) =
  let (from1, to1) = range e1
      (from2, to2) = range e2
  in
    if from1 < 0 || to1 < 0 || from2 < 0 || to2 < 0
    then error ("bad range " ++ show (from1, to1, from2, to2))
    else (div from1 to2, div to1 from2)
range (Mod e1 e2) =
  let (from1, to1) = range e1
      (from2, to2) = range e2
  in
    if from1 < 0 || to1 < 0 || from2 < 0 || to2 < 0
    then error ("bad range " ++ show (from1, to1, from2, to2))
    else if to1 < to2 then (from1, from2) else (0, to2)
range (Eql e1 e2) =
  let (from1, to1) = range e1
      (from2, to2) = range e2
  in if to1 < from2 || to2 < from1 then (0, 0) else (0, 1)

zeroAccu :: Accu
zeroAccu = Accu 0 (Literal 0) (Literal 0) (Literal 0) (Literal 0)

maybeGetRegister :: Accu -> String -> Maybe Expression
maybeGetRegister (Accu _ w _ _ _) "w" = Just w
maybeGetRegister (Accu _ _ x _ _) "x" = Just x
maybeGetRegister (Accu _ _ _ y _) "y" = Just y
maybeGetRegister (Accu _ _ _ _ z) "z" = Just z
maybeGetRegister _ _ = Nothing

getRegister :: Accu -> String -> Expression
getRegister accu str =
  let result = maybeGetRegister accu str
  in case result of
     Just expr -> expr
     Nothing -> error ("bad register " ++ str)

updateRegister :: Accu -> String -> Expression -> Accu
updateRegister (Accu pos _ x y z) "w" expr = Accu pos expr x y z
updateRegister (Accu pos w _ y z) "x" expr = Accu pos w expr y z
updateRegister (Accu pos w x _ z) "y" expr = Accu pos w x expr z
updateRegister (Accu pos w x y _) "z" expr = Accu pos w x y expr
updateRegister _ s _ = error ("bad register " ++ s)

advancePos :: Accu -> Accu
advancePos (Accu pos w x y z) = Accu (pos + 1) w x y z

getPos :: Accu -> Int
getPos (Accu pos _ _ _ _ ) = pos

readOps :: FilePath -> IO Accu
readOps path = do
  contents <- readFile path
  let myLines = lines contents
  let accu = foldl' toExpr zeroAccu myLines
  return accu

toExpr :: Accu -> String -> Accu
toExpr accu str =
  let tokens = words str
      op = head tokens
      a = tokens !! 1
  in toExpr2 accu op a (drop 2 tokens)

reduceFully :: Expression -> Expression
reduceFully expr =
  let (reduced, newExpr) = reduceExpr expr
  in
    if not reduced then
      let (reducedRange, newExprRange) = reduceOnRange expr
      in
        if not reducedRange then -- trace ("not reduced " ++ show expr)
        expr
        else -- trace ("range reduced " ++ show expr ++ " to " ++ show newExprRange)
        reduceFully newExprRange
    else -- trace ("reduced " ++ show expr ++ " to " ++ show newExpr)
    reduceFully newExpr

reduceOnRange :: Expression -> (Bool, Expression)
reduceOnRange e@(Literal _) = (False, e)
reduceOnRange e@(Inp _) = (False, e)

reduceOnRange expr =
 let (low, high) = range expr
  in
    if low == high then (True, Literal low)
    else case expr of
      e@(Mod e1 lit@(Literal n)) ->
        let (_, high1) = range e1
            (r, newE1) = reduceOnRange e1
        in
          if high1 < n then (True, newE1)
          else if not r then (False, e) else (True, Mod newE1 lit)
      e@(Add e1 e2) -> reduceOnRangeSubs e e1 e2 Add
      e@(Mul e1 e2) -> reduceOnRangeSubs e e1 e2 Mul
      e@(Div e1 e2) -> reduceOnRangeSubs e e1 e2 Div
      e@(Mod e1 e2) -> reduceOnRangeSubs e e1 e2 Mod
      e@(Eql e1 e2) -> reduceOnRangeSubs e e1 e2 Eql
      e -> (False, e)

reduceOnRangeSubs :: Expression -> Expression -> Expression ->(Expression -> Expression -> Expression) -> (Bool, Expression)
reduceOnRangeSubs e e1 e2 ctor =
  let (r1, newE1) = reduceOnRange e1
      (r2, newE2) = reduceOnRange e2
      r = r1 || r2
  in (r, if r then ctor newE1 newE2 else e)

reduceExpr :: Expression -> (Bool, Expression)
reduceExpr (Add (Literal a) (Literal b)) = (True, Literal (a + b))
reduceExpr (Mul (Literal a) (Literal b)) = (True, Literal (a * b))
reduceExpr (Div (Literal a) (Literal b)) = (True, Literal (div a b))
reduceExpr (Mod (Literal a) (Literal b)) = (True, Literal (mod a b))
reduceExpr (Eql (Literal a) (Literal b)) = (True, Literal (if a == b then 1 else 0))

reduceExpr (Add expr (Literal 0)) = (True, snd (reduceExpr expr))
reduceExpr (Add (Literal 0) expr) = (True, snd (reduceExpr expr))
reduceExpr (Add (Add e1 (Literal n1)) (Literal n2)) = (True, Add (snd (reduceExpr e1)) (Literal (n1 + n2)))
reduceExpr (Add (Add (Literal n1) e1) (Literal n2)) = (True, Add (snd (reduceExpr e1)) (Literal (n1 + n2)))

reduceExpr (Mul _ (Literal 0)) = (True, Literal 0)
reduceExpr (Mul (Literal 0) _) = (True, Literal 0)
reduceExpr (Mul expr (Literal 1)) = (True, snd (reduceExpr expr))
reduceExpr (Mul (Literal 1) expr) = (True, snd (reduceExpr expr))
reduceExpr (Mul (Mul e1 (Literal n1)) (Literal n2)) = (True, Mul (snd (reduceExpr e1)) (Literal (n1 * n2)))
reduceExpr (Mul (Mul (Literal n1) e1) (Literal n2)) = (True, Mul (snd (reduceExpr e1)) (Literal (n1 * n2)))

reduceExpr (Mul (Add e1 (Literal n1)) (Literal n2)) = (True, Add (Mul (snd (reduceExpr e1)) (Literal n2)) (Literal (n1 * n2)))

reduceExpr (Div expr (Literal 1)) = (True, snd (reduceExpr expr))
reduceExpr (Div (Literal 0) _) = (True, Literal 0)

reduceExpr (Mod _ (Literal 1)) = (True, Literal 0)
reduceExpr (Mod (Literal 0) _) = (True, Literal 0)

reduceExpr (Eql (Literal 1) (Eql e1 e2)) = (True, Eql e1 e2)
reduceExpr (Eql (Eql e1 e2) (Literal 1)) = (True, Eql e1 e2)

reduceExpr e@(Add e1 e2) = reduceSubs e e1 e2 Add
reduceExpr e@(Mul e1 e2) = reduceSubs e e1 e2 Mul
reduceExpr e@(Div e1 e2) = reduceSubs e e1 e2 Div
reduceExpr e@(Mod e1 e2) = reduceSubs e e1 e2 Mod
reduceExpr e@(Eql e1 e2) = reduceSubs e e1 e2 Eql

reduceExpr e@(Inp _) = (False, e)
reduceExpr e@(Literal _) = (False, e)

reduceSubs :: Expression -> Expression -> Expression -> (Expression -> Expression -> Expression) -> (Bool, Expression)
reduceSubs e e1 e2 ctor =
  let (r1, newE1) = reduceExpr e1
      (r2, newE2) = reduceExpr e2
      r = r1 || r2
  in (r, if r then ctor newE1 newE2 else e)

toExpr2 :: Accu -> String -> String -> [String] -> Accu
toExpr2 accu "inp" reg _ =
  advancePos (updateRegister accu reg (Inp (getPos accu)))
toExpr2 accu "add" reg tokens =
  let expr = Add (getRegister accu reg) (toRegOrLiteral accu (head tokens))
  in updateRegister accu reg (reduceFully expr)
toExpr2 accu "mul" reg tokens =
  let expr = Mul (getRegister accu reg) (toRegOrLiteral accu (head tokens))
  in updateRegister accu reg (reduceFully expr)
toExpr2 accu "div" reg tokens =
  let expr = Div (getRegister accu reg) (toRegOrLiteral accu (head tokens))
  in updateRegister accu reg (reduceFully expr)
toExpr2 accu "mod" reg tokens =
  let expr = Mod (getRegister accu reg) (toRegOrLiteral accu (head tokens))
  in updateRegister accu reg (reduceFully expr)
toExpr2 accu "eql" reg tokens =
  let expr = Eql (getRegister accu reg) (toRegOrLiteral accu (head tokens))
  in updateRegister accu reg (reduceFully expr)
toExpr2 _ s _ _ =
  error ("unknown op " ++ s)

toRegOrLiteral :: Accu -> String -> Expression
toRegOrLiteral accu str =
  let reg = maybeGetRegister accu str
  in case reg of
      Just expr -> expr
      Nothing -> Literal (readInt str)

exec :: Expression -> [Char] -> Int
exec (Inp index) str = digitToInt (str !! index)
exec (Literal n) _ = n
exec (Add e1 e2) str = exec e1 str + exec e2 str
exec (Mul e1 e2) str = exec e1 str * exec e2 str
exec (Div e1 e2) str = div (exec e1 str) (exec e2 str)
exec (Mod e1 e2) str = mod (exec e1 str) (exec e2 str)
exec (Eql e1 e2) str = if exec e1 str == exec e2 str then 1 else 0
