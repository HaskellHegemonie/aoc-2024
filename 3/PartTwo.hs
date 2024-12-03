module Main where
import Data.List.Split
import Data.Char
import Control.Monad
import Data.Maybe
import Control.Arrow
import Data.List

main = do
  s <- readFile "in.txt"
  -- s <- readFile "realinput.txt"
  print $ logic . parse $ s
  pure ()

parse = id
logic xs = sum $ map (fromMaybe 0) $ snd comma
  where
    mul = splitOn "mul(" xs
    brace = map (id &&& safeHead . splitOn ")") mul
    comma = mapAccumL (\b (next, mx) -> (,) (logic' next b) $ do
                    guard b
                    x <- splitOn "," <$> mx
                    y <- safeTake2 x
                    guard (all (all isNumber) y)
                    pure $ product $ map read y
                )
            True
            brace

logic' :: String -> Bool -> Bool
logic' xs c = case compare a b of
  EQ -> c
  LT -> False
  GT -> True
  where
    z = tails $ reverse xs
    a = fromMaybe maxBound $ findIndex (\x -> take 7 x == ")(t'nod") z
    b = fromMaybe maxBound $ findIndex (\x -> take 4 x == ")(od") z


safeTake2 (x:y:_) = Just [x, y]
safeTake2 _ = Nothing
safeHead (x:_) = Just x
safeHead _ = Nothing
