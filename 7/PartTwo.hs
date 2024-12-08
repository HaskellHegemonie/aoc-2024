module Main where
import Data.List.Split
import Data.List

main = do
  -- s <- readFile "in.txt"
  s <- readFile "realinput.txt"
  print $ logic . parse $ s
  pure ()

parse :: String -> [(Int, [Int])]
parse xs = zip (map read res) t
  where
    toTuple [x, y] = (x, y)
    (res, t') = unzip $ map (toTuple . splitOn ": ") $ lines xs
    t = map (map read . words) t'

logic = sum . mconcat . map (\(res, xs) -> take 1 $ filter (== res) $ v $ reverse xs)

v [] = []
v [x] = [x]
v (x:xs) = concatMap (\n -> [x + n, x * n, conc n x]) next
 where
   next = v xs

conc :: Int -> Int -> Int
conc x y = read $ show x <> show y
