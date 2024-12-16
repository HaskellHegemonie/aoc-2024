module Main where
import Data.Function
main = do
  -- s <- readFile "in.txt"
  s <- readFile "realinput.txt"
  s & parse & logic & print
parse :: String -> [Int]
parse xs = xs & lines & head & words & map read

logic xs = length $ iterate (concatMap handleNum) xs !! 25
handleNum :: Int -> [Int]
handleNum 0 = [1]
handleNum x
 | even (length asStr) = map read [one, two]
 | True = [x * 2024]
   where
     asStr = show x
     (one, two) = splitAt (div (length asStr) 2) asStr
