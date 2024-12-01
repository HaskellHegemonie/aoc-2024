module Main where
import Aux
import Data.List (sort)

main :: IO ()
main = do
  s <- readFile $ "in.txt"
  -- s <- readFile $ "realinput.txt"
  print $ logic . parse $ s
  pure ()

parse :: String -> [[Int]]
parse = transpose . map (map read . words) . lines

logic l = innerFun x y 0
  where
    [x, y'] = map sort l
    y = map (length &&& head) $ group y'
    innerFun [] _ aux = aux
    innerFun _ [] aux = aux
    innerFun lx@(x:xs) ly@((l,y):ys) aux =
      case compare x y of
        EQ -> innerFun xs ly (aux + l * x)
        GT -> innerFun lx ys aux
        LT -> innerFun xs ly aux
