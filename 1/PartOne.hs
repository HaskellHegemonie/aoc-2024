module Main where
import Aux
import Data.List (sort)
-- import Data.Monoid

main :: IO ()
main = do
  s <- readFile $ "in.txt"
  -- s <- readFile $ "realinput.txt"
  print $ logic . parse $ s
  pure ()


parse :: String -> [[Int]]
parse = transpose .  map (map read . words) . lines
logic = getSum . foldMap (Sum . abs) . uncurry (zipWith (-)) . (\[x,y] -> (x, y)) . map sort
