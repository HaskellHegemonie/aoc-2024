module Main where
import Data.Array
import Data.Function
import Control.Arrow
import Data.Set qualified as S
import Debug.Trace
type Position = (Int, Int)
type Arr = Array Position Char

main = do
  -- s <- readFile "in.txt"
  s <- readFile "realinput.txt"
  let
    sp = s & parse
    sl = sp & logic
  -- print $ sp
  print $ sl

parse :: String -> (Arr, [Position])
parse xs = (arr, zeroes)
  where
    l = lines xs
    lements = mconcat l
    arr = listArray ((0, 0), pred *** pred $ (length l, length (head l))) lements
    zeroes = assocs arr & filter (\(i, e) -> e == '0') & map fst


logic :: (Arr, [Position]) -> Int
logic (arr, pos) = pos & map (fst . getScore arr S.empty) & sum
getScore :: Arr -> S.Set Position -> Position -> (Int, S.Set Position)
getScore arr s p@(x, y) = if S.member p s then (0, s) else
  if arr ! p == '9'
  then (1, S.insert p s)
  else surr & foldl
  (\(res, b) p' ->
     let
       nb = S.insert p b
       (res', b') = getScore arr nb p'
     in
     if read [arr ! p'] == 1 + read [arr ! p]
     then (res' + res, b')
     else (res, nb)
  ) (0, s)
  where
    surr = genValidSurround arr p

genValidSurround :: Arr -> Position -> [Position]
genValidSurround arr p@(x, y) = pos &
  map (\(dx, dy) -> (x + dx, y + dy)) &
  filter (inRange b) &
  filter (\p -> arr ! p /= '.')
  where
    pos = [(x, y) | x <- [-1..1], y <- [-1..1], abs x /= abs y]
    b = bounds arr
