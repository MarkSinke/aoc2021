module Day24 (day24a, day24b) where

import Common
import Data.Char (digitToInt)
import Data.List (foldl')
import Debug.Trace

day24a :: IO Int
day24a = do
  accu <- readOps "/Users/marksinke/IdeaProjects/aoc2021/data/day24input.txt"
  let z = getRegister accu "z"
  print accu
  print (range (getRegister accu "x"))
  return 0
--  let nums = [99999999999999, 99999999999998..11111111111111]
--  let result = z `seq` find (isZero z) nums
--  print result
--  return (getPos accu)

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
  Eql Expression Expression Expression Expression deriving (Eq) -- lhs == rhs then else

instance Show Expression where
   show (Literal n) = show n
   show (Inp n) = "i" ++ show (n + 1)
   show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
   show (Mul e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
   show (Div e1 e2) = "(" ++ show e1 ++ " div " ++ show e2 ++ ")"
   show (Mod e1 e2) = "(" ++ show e1 ++ " mod " ++ show e2 ++ ")"
   show (Eql e1 e2 t e) = "(if " ++ show e1 ++ " == " ++ show e2 ++ "\nthen {" ++ show t ++ "}\nelse {" ++ show e ++ "})"

data Accu = Accu Int Expression Expression Expression Expression -- inputPos w x y z

instance Show Accu where
   show (Accu pos w x y z) = "pos=" ++ show pos ++ "\nw = " ++ show w ++ "\nx = " ++ show x ++ "\ny = " ++ show y ++ "\nz = " ++ show z

data ExpressionContext = ExpressionContext [(Expression, Expression)] [(Expression, Expression)] -- known trues, known falses

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
    else if to1 < from2 then (from1, to1) else (0, to2)
range (Eql _ _ t el) =
  let (from1, to1) = range t
      (from2, to2) = range el
  in (min from1 from2, max to1 to2)

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
  let (_, accu) = foldl' toExpr (0, zeroAccu) myLines
  return accu

toExpr :: (Int, Accu) -> String -> (Int, Accu)
toExpr (pos, accu) str =
  let tokens = words str
      op = head tokens
      a = tokens !! 1
      result = (pos + 1, toExpr2 accu op a (drop 2 tokens))
  in traceShow (str, result) result

reduceFully :: Expression -> Expression
reduceFully expr =
  let context = ExpressionContext [] []
      (reduced, newExpr) = reduceExpr context expr
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
      e@(Eql e1 e2 t el) ->
        let (from1, to1) = range e1
            (from2, to2) = range e2
        in
          if to1 < from2 || to2 < from1 then (True, snd (reduceOnRange el))
          else if from1 == to1 && from2 == to2 && from1 == from2 then (True, snd (reduceOnRange t))
          else let (rT, newT) = reduceOnRange t
                   (rEl, newEl) = reduceOnRange el
                   rR = rT || rEl
               in (rR, if rR then Eql e1 e2 newT newEl else e)

      e -> (False, e)

reduceOnRangeSubs :: Expression -> Expression -> Expression ->(Expression -> Expression -> Expression) -> (Bool, Expression)
reduceOnRangeSubs e e1 e2 ctor =
  let (r1, newE1) = reduceOnRange e1
      (r2, newE2) = reduceOnRange e2
      r = r1 || r2
  in (r, if r then ctor newE1 newE2 else e)

reduceExpr :: ExpressionContext -> Expression -> (Bool, Expression)
reduceExpr _ (Add (Literal a) (Literal b)) = (True, Literal (a + b))
reduceExpr _ (Mul (Literal a) (Literal b)) = (True, Literal (a * b))
reduceExpr _ (Div (Literal a) (Literal b)) = (True, Literal (div a b))
reduceExpr _ (Mod (Literal a) (Literal b)) = (True, Literal (mod a b))

reduceExpr c (Add expr (Literal 0)) = (True, snd (reduceExpr c expr))
reduceExpr c (Add (Literal 0) expr) = (True, snd (reduceExpr c expr))
reduceExpr c (Add (Add e1 (Literal n1)) (Literal n2)) = (True, Add (snd (reduceExpr c e1)) (Literal (n1 + n2)))
reduceExpr c (Add (Add (Literal n1) e1) (Literal n2)) = (True, Add (snd (reduceExpr c e1)) (Literal (n1 + n2)))

reduceExpr _ (Mul _ (Literal 0)) = (True, Literal 0)
reduceExpr _ (Mul (Literal 0) _) = (True, Literal 0)
reduceExpr c (Mul expr (Literal 1)) = (True, snd (reduceExpr c expr))
reduceExpr c (Mul (Literal 1) expr) = (True, snd (reduceExpr c expr))
reduceExpr c (Mul (Mul e1 (Literal n1)) (Literal n2)) = (True, Mul (snd (reduceExpr c e1)) (Literal (n1 * n2)))
reduceExpr c (Mul (Mul (Literal n1) e1) (Literal n2)) = (True, Mul (snd (reduceExpr c e1)) (Literal (n1 * n2)))

--reduceExpr c (Mul (Add e1 (Literal n1)) (Literal n2)) = (True, Add (Mul (snd (reduceExpr e1)) (Literal n2)) (Literal (n1 * n2)))

reduceExpr c (Div expr (Literal 1)) = (True, snd (reduceExpr c expr))
reduceExpr _ (Div (Literal 0) _) = (True, Literal 0)

reduceExpr _ (Mod _ (Literal 1)) = (True, Literal 0)
reduceExpr _ (Mod (Literal 0) _) = (True, Literal 0)

-- mod (e11 + n * X) n or one of its 3 other permutations -> mod e11 n
reduceExpr c e@(Mod e1@(Add e11 (Mul (Literal n1) _)) e2@(Literal n2)) =
  if n1 == n2 then (True, Mod e11 e2) else reduceSubs c e e1 e2 Mod
reduceExpr c e@(Mod e1@(Add e11 (Mul _ (Literal n1))) e2@(Literal n2)) =
  if n1 == n2 then (True, Mod e11 e2) else reduceSubs c e e1 e2 Mod
reduceExpr c e@(Mod e1@(Add (Mul _ (Literal n1)) e12) e2@(Literal n2)) =
  if n1 == n2 then (True, Mod e12 e2) else reduceSubs c e e1 e2 Mod
reduceExpr c e@(Mod e1@(Add (Mul (Literal n1) _) e12) e2@(Literal n2)) =
  if n1 == n2 then (True, Mod e12 e2) else reduceSubs c e e1 e2 Mod

-- div (e11 + n * e12) n -> div e11 n + e12
reduceExpr c e@(Div e1@(Add e11 (Mul (Literal n1) e12)) e2@(Literal n2)) =
  if n1 == n2 then (True, Add (Div e11 e2) e12) else reduceSubs c e e1 e2 Div
reduceExpr c e@(Div e1@(Add e11 (Mul e12 (Literal n1))) e2@(Literal n2)) =
  if n1 == n2 then (True, Add (Div e11 e2) e12) else reduceSubs c e e1 e2 Div
reduceExpr c e@(Div e1@(Add (Mul (Literal n1) e11) e12) e2@(Literal n2)) =
  if n1 == n2 then (True, Add e11 (Div e12 e2)) else reduceSubs c e e1 e2 Div
reduceExpr c e@(Div e1@(Add (Mul e11 (Literal n1)) e12) e2@(Literal n2)) =
  if n1 == n2 then (True, Add e11 (Div e12 e2)) else reduceSubs c e e1 e2 Div

-- reduceExpr c (Eql (Literal 1) (Eql e1 e2)) = (True, Eql e1 e2)
-- reduceExpr c (Eql (Eql e1 e2) (Literal 1)) = (True, Eql e1 e2)
reduceExpr _ (Eql (Eql e11 e12 _ (Literal 0)) (Literal 0) t el) = (True, Eql e11 e12 el t)
reduceExpr _ (Eql (Literal 0) (Eql e11 e12 _ (Literal 0)) t el) = (True, Eql e11 e12 el t)

reduceExpr c e@(Mul e1@(Eql e11 e12 t1 el1) e2@(Eql e21 e22 t2 el2)) =
  let same = e11 == e21 && e12 == e22
  in
    if same then (True, Eql e11 e12 (Mul t1 t2) (Mul el1 el2))
    else reduceSubs c e e1 e2 Mul

reduceExpr _ (Mul (Eql e11 e12 t el) e2) = (True, Eql e11 e12 (Mul t e2) (Mul el e2))
reduceExpr _ (Mul e1 (Eql e21 e22 t el)) = (True, Eql e21 e22 (Mul e1 t) (Mul e1 el))

reduceExpr c e@(Add e1@(Eql e11 e12 t1 el1) e2@(Eql e21 e22 t2 el2)) =
  let same = e11 == e21 && e12 == e22
  in
    if same then (True, Eql e11 e12 (Add t1 t2) (Add el1 el2))
    else reduceSubs c e e1 e2 Add

reduceExpr _ (Add (Eql e11 e12 t el) e2) = (True, Eql e11 e12 (Add t e2) (Add el e2))
reduceExpr _ (Add e1 (Eql e21 e22 t el)) = (True, Eql e21 e22 (Add e1 t) (Add e1 el))

reduceExpr c e@(Div e1@(Eql e11 e12 t1 el1) e2@(Eql e21 e22 t2 el2)) =
  let same = e11 == e21 && e12 == e22
  in
    if same then (True, Eql e11 e12 (Div t1 t2) (Div el1 el2))
    else reduceSubs c e e1 e2 Div

reduceExpr _ (Div (Eql e11 e12 t el) e2) = (True, Eql e11 e12 (Div t e2) (Div el e2))
reduceExpr _ (Div e1 (Eql e21 e22 t el)) = (True, Eql e21 e22 (Div e1 t) (Div e1 el))

reduceExpr c e@(Mod e1@(Eql e11 e12 t1 el1) e2@(Eql e21 e22 t2 el2)) =
  let same = e11 == e21 && e12 == e22
  in
    if same then (True, Eql e11 e12 (Mod t1 t2) (Mod el1 el2))
    else reduceSubs c e e1 e2 Mod

reduceExpr _ (Mod (Eql e11 e12 t el) e2) = (True, Eql e11 e12 (Mod t e2) (Mod el e2))
reduceExpr _ (Mod e1 (Eql e21 e22 t el)) = (True, Eql e21 e22 (Mod e1 t) (Mod e1 el))

reduceExpr c e@(Add e1 e2) = reduceSubs c e e1 e2 Add
reduceExpr c e@(Mul e1 e2) = reduceSubs c e e1 e2 Mul
reduceExpr c e@(Div e1 e2) = reduceSubs c e e1 e2 Div
reduceExpr c e@(Mod e1 e2) = reduceSubs c e e1 e2 Mod
reduceExpr c e@(Eql e1 e2 t el) = reduceEql c e e1 e2 t el

reduceExpr _ e@(Inp _) = (False, e)
reduceExpr _ e@(Literal _) = (False, e)

reduceEql :: ExpressionContext -> Expression -> Expression -> Expression -> Expression -> Expression -> (Bool, Expression)
reduceEql c@(ExpressionContext trueList falseList) e e1 e2 t el =
  let (r1, newE1) = reduceExpr c e1
      (r2, newE2) = reduceExpr c e2
      cond = (newE1, newE2)
      (rT, newT) = reduceExpr (ExpressionContext (cond : trueList) falseList) t
      (rEl, newEl) = reduceExpr (ExpressionContext trueList (cond : falseList)) el
      r = r1 || r2 || rT || rEl
  in
    if cond `elem` trueList then (True, newT)
    else if cond `elem` falseList then (True, newEl)
    else (r, if r then Eql newE1 newE2 newT newEl else e)

reduceSubs :: ExpressionContext -> Expression -> Expression -> Expression -> (Expression -> Expression -> Expression) -> (Bool, Expression)
reduceSubs c e e1 e2 ctor =
  let (r1, newE1) = reduceExpr c e1
      (r2, newE2) = reduceExpr c e2
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
  let expr = Eql (getRegister accu reg) (toRegOrLiteral accu (head tokens)) (Literal 1) (Literal 0)
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
exec (Eql e1 e2 t e) str = if exec e1 str == exec e2 str then exec t str else exec e str

