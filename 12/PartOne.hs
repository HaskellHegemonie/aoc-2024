module Main where
import Control.Arrow
import Data.Function
import Data.Array
import Data.List
import Data.Map qualified as M
import Data.Set qualified as S
import Debug.Trace

type Coord = (Int, Int)
type Arr = Array Coord Char
type Map = M.Map Coord Int

main = do
  -- s <- readFile "in.txt"
  -- s <- readFile "in1.txt"
  s <- readFile "in2.txt"
  let
    ps = s & parse
    ls = ps & logic
  ps & print
  ls & print
  pure ()

parse xs = arr
  where
    l = lines xs
    ml = l & mconcat
    arr = listArray ((0, 0), pred *** pred $ (length l, length (head l))) ml

logic arr = l
  where
    b@(_, (bx, by)) = bounds arr
    coords = [(x, y) | x <- [0..bx], y <- [0..by]]
    l = coords & foldl fun (S.empty, 0)
    fun (bs, res) a =
       if S.member a bs
       then (bs', res)
       else (S.union bs' $ S.fromList f, res + res')
       where
         bs' = S.insert a bs
         (f, s) = unzip $ M.toList $ logic' arr M.empty a
         res' = traceShowId $ calculate s
         calculate xs = length xs * sum xs

logic' :: Arr -> Map -> Coord -> Map
logic' arr m p = case M.lookup p m of
  Just _ -> m
  Nothing -> foldl (\b a -> logic' arr b a) m' next
    where
      (next, perimiter) = surround arr p
      m' = M.insert p (length perimiter) m

surround :: Arr -> Coord -> ([Coord], [Coord])
surround arr p@(x, y) = partition (\x -> if inRange (bounds arr) x then arr ! x == arr ! p else False) sl
  where
   sl = [ np | dx <- [-1..1], dy <- [-1..1],
          abs dx /= abs dy,
          let np@(nx, ny) = (x + dx, y + dy)
             ]
