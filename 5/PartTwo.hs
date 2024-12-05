module Main where
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Map qualified as M
import Data.Set qualified as S

main = do
  s <- readFile "in.txt"
  -- s <- readFile "realinput.txt"
  print $ uncurry logic . parse $ s

data Holds = Holds { predecessors :: [Int], successors :: [Int] } deriving Show
holdsUnion (Holds b0 a0) (Holds b1 a1) = Holds (b0 <> b1) $ a0 <> a1

parse :: String -> (M.Map Int Holds, [[Int]])
parse xs = (entriesMap, updates)
  where
    l = lines xs
    [entries', updates'] = splitOn [""] l
    inner x = map read . splitOn x
    entries = map (inner "|") entries'
    entriesMap = foldl (\b [bef, aft] ->
                          M.insertWith holdsUnion
                          aft (Holds [bef] []) $
                          M.insertWith holdsUnion
                          bef (Holds [] $ [aft]) b
                          ) M.empty entries
    updates = map (inner ",") updates'

l m (prev, xs) curr = (,) next $ fromMaybe xs (fst <$> corrected)
  where
    next = prev <> [curr]
    corrected = do
      l <- M.lookup curr m
      let meta = intersect prev $ successors l
      pure $ (foldl (\b int -> fromMaybe b $ do
        iidx <- findIndex (== int) b
        curridx <- findIndex (== curr) b
        pure $ swap (iidx, curridx) b
              ) xs $ meta, meta)
    swap (x, y) xs = [get z e | (z, e) <- zip [0..] xs]
        where
          get z e
                | z == x = xs !! y
                | z == y = xs !! x
                | True = e

getIfValid m xs = if correct == xs then 0 else correct !! div (length xs) 2
  where
    correct = iterate (\xs -> snd $ foldl (l m) ([], xs) xs) xs !! 20
    -- ↑ don't tink about it too long. I will come back to this when I have more free time than now
    -- Also don't think about why I store the predecessors if I don't use it
    -- Also don't wonder where the HashSet went
logic m xs = sum $ map (getIfValid m) xs

safeHead (x:_) = Just x
safeHead _ = Nothing
