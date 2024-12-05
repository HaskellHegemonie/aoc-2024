module Main where
import Data.List.Split
import Control.Monad
import Control.Applicative
import Data.Map qualified as M
import Data.Set qualified as S

main = do
  s <- readFile "in.txt"
  -- s <- readFile "realinput.txt"
  print $ uncurry logic . parse $ s

data Holds = Holds { predecessors :: S.Set Int, successors :: S.Set Int } deriving Show
holdsUnion (Holds b0 a0) (Holds b1 a1) = Holds (S.union b0 b1) $ S.union a0 a1

parse :: String -> (M.Map Int Holds, [[Int]])
parse xs = (entriesMap, updates)
  where
    l = lines xs
    [entries', updates'] = splitOn [""] l
    inner x = map read . splitOn x
    entries = map (inner "|") entries'
    entriesMap = foldl (\b [bef, aft] ->
                          M.insertWith holdsUnion
                          aft (Holds (S.singleton bef) S.empty) $
                          M.insertWith holdsUnion
                          bef (Holds S.empty $ S.singleton aft) b
                          ) M.empty entries
    updates = map (inner ",") updates'

l m (prev, res) curr = (,) (S.union prev $ S.singleton curr) $ do
  _ <- res
  h <- st
  guard $ 0 == (length $ S.intersection prev $ successors h)
  where
    st = M.lookup curr m <|> Just (Holds S.empty S.empty)
getIfValid m xs = if Nothing /= snd (foldl (l m) (S.empty, Just ()) xs)
  then xs !! (div (length xs) 2)
  else 0
logic m xs = sum $ map (getIfValid m) xs
