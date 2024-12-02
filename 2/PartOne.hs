module Main where

main = do
  s <- readFile "in.txt"
  -- s <- readFile "realinput.txt"
  print $ logic . parse $ s
  pure ()

parse :: String -> [[Int]]
parse = map (map read . words) . lines

-- Does not work for sequences that contain 0 or negative numbers.
-- But the input file doesn't contain these cases (although these constraints are not mentioned in the text)
isSafe xs =
  all (== signum t) (map signum ts)
  &&
  all (\x' -> let x = abs x' in x >= 1 && x <= 3) lt
  where
    lt@(t:ts) = zipWith (-) xs (tail xs)

logic = sum . map (fromEnum . isSafe)
