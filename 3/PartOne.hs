module Main where
import Data.List.Split
import Data.Char
import Control.Monad
import Data.Functor
import Data.Maybe

main = do
  s <- readFile "in.txt"
  -- s <- readFile "realinput.txt"
  print $ logic . parse $ s
  pure ()

parse = id
logic xs = sum $ map (fromMaybe 0) comma
  where
    mul = splitOn "mul(" xs
    brace = map (safeHead . splitOn ")") mul
    comma = map (\mx -> do
                    x <- splitOn "," <$> mx
                    y <- safeTake2 x
                    guard (all (all isNumber) y)
                    pure $ product $ map read y
                )
            brace
    
safeTake2 (x:y:_) = Just [x, y]
safeTake2 _ = Nothing
safeHead (x:_) = Just x
safeHead _ = Nothing
