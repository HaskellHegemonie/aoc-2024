module Main where
import Data.Array
import Data.Char
import Data.List
import Data.Function
import Data.Set qualified as S

main = do
  -- s <- readFile "example.txt"
  -- s <- readFile "in.txt"
  s <- readFile "realinput.txt"
  let
    (arr, loc) = parse s
    b = bounds arr
  print $ logic arr $ loc
  pure ()

n = negate
parse xs = (arr, locations)
  where
    l = lines xs
    arr = listArray ((0, 0), (length l + n 1, n 1 + length (head l))) $ mconcat l
    locations =
      map (map fst) $ groupBy (on (==) snd) $ sortBy (on compare snd) $
      concatMap (\(x, xs) -> let (y, a) = unzip xs in zip (map ((,) x) y) a) $
      filter ((/= []) . snd) $ map (\(n, a) -> (,) n $
                                     filter (\(_, b) -> any ($ b) [isNumber, isAlpha]) a) $
      map (\(n, a) -> (n, zip [0..] a)) $ zip [0..] l

logic arr xs = length $ foldl (\m a -> innerLogic a m) S.empty xs
  where
    b@(zero, (mx, my)) = bounds arr
    innerLogic [] m = m
    innerLogic ((cx, cy):ys) m = innerLogic ys (foldl (\m (dx, dy) ->
                                      let
                                        distx = cx - dx
                                        disty = cy - dy
                                        new = iterate (cx + distx, cy + disty)
                                        new' = (dx - distx, dy - disty)
                                      in
                                        foldr S.insert m $ filter (\x -> inRange b x) [new, new']
                                      ) m
                                      ys)
