module Main where
import Data.List.Split

main = do
  s <- readFile "in.txt"
  -- s <- readFile "realinput.txt"
  print $ logic . parse $ s
  pure ()

parse :: String -> [(Int, [Int])]
parse xs = zip (map read res) t
  where
    toTuple [x, y] = (x, y)
    (res, t') = unzip $ map (toTuple . splitOn ": ") $ lines xs
    t = map (map read . words) t'

logic = sum . mconcat . map (\(res, (x:xs)) -> z xs [x] res)
z [] prev want = take 1 $ filter (== want) prev
z (x:xs) prev want = z xs (concatMap (\cur -> [x + cur, x * cur]) prev) want
