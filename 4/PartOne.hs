module Main where
import Data.Array
import Data.Maybe
import Debug.Trace

main = do
  s <- readFile "in.txt"
  -- s <- readFile "realinput.txt"
  print $ uncurry (logic 0 0) . parse $ s
  pure ()

n = negate
parse :: String -> ([String], Array (Int, Int) Char)
parse input = (inputLines, arr)
  where
    inputLines = lines input
    arr = listArray ((0, 0), (length inputLines + n 1, length (head inputLines) + n 1))
      $ mconcat inputLines

getValids :: (Int, Int) -> Array (Int, Int) Char -> Int
getValids (r, c) arr = res
  where
    res = sum $ map (fromEnum . fromMaybe False . isValidHelper arr) [horiz, vert, diag1, diag2]
    horiz = map (\d -> [r, c + d]) [0..3]
    vert = map (\d -> [r + d, c]) [0..3]
    diag1 = map (\d -> [r + d, c + d]) [0..3]
    diag2 = map (\d -> [r + n d, c + d]) [0..3]

isValidHelper arr xs =  isXmas <$> traverse (
  \[x, y] ->
    if inRange b (x, y)
    then Just $ arr ! (x, y)
    else Nothing ) xs
  where
    isXmas x = any (== "XMAS") [id x, reverse x]
    b = bounds arr

logic aux _ [] _  = aux
logic aux r (x:xs) arr = logic (aux + rowResult) (r + 1) xs arr
  where
    rowResult = sum $ map (\(c, _) -> getValids (r, c) arr) $ zip [0..] x
