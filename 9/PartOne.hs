module Main where
import Data.Function
import Data.Functor

main = do
  s <- readFile "in.txt"
  s <- readFile "realinput.txt"
  let
    p = parse s
  print $ logic p
  pure ()

parse :: String -> [Int]
parse xs =
  xs &
  init <&>
  read . pure


logic xs =
  zip xs [0..] <&>
  (\(x, z) -> (if odd z then ('.', 0) else (' ', div z 2)) & replicate x) &
  mconcat &
  f &
  zip [0..] &
  map (\(x, y) -> x * y) &
  sum


f [] = []
f [('.', _)] = []
f (('.',_):xs) =  f (l : init xs)
  where
    l = last xs
f ((' ', x):xs) = x : f xs
