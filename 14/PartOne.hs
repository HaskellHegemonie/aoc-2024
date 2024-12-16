module Main where
import Data.List
import Data.List.Split
import Control.Arrow
import Data.Function
import Data.Functor
type Position = (Int, Int)
type Velocity = (Int, Int)
data Quadrant = First | Second | Third | Fourth deriving (Show, Eq, Ord)
-- b@(bx, by) = (7, 11)
b@(bx, by) = (103, 101)
seconds = 100

main = do
  -- s <- readFile "in.txt"
  s <- readFile "realinput.txt"
  let
    ps = s & parse
    ls = ps & logic
  ps & take 20 & print
  ls & print
  pure ()

parse :: String -> [((Int, Int), (Int, Int))]
parse xs = map p l
  where
    l = lines xs
    p l = l &
      splitOn " " <&>
      (\x -> x &
       splitOn "," &
       (\(x:y:_) -> read *** read $ (drop 2 x, y)) &
       (\(x, y) -> (y, x))
      ) &
      (\(x:y:_) -> (x, y))

positionElapsed :: Position -> Velocity -> Position
positionElapsed (x, y) (vx, vy) = f (x + vx * seconds, y + vy * seconds)
 where
   f :: Position -> Position
   f = (`mod` bx) *** (`mod` by)

inMiddle (x, y) = div bx 2 == x || div by 2 == y
getQuadrant (x, y)
 | x < midx && y > midy = First
 | x < midx && y < midy = Second
 | x > midx && y < midy = Third
 | True = Fourth

 where
   midx = div bx 2
   midy = div by 2


test = positionElapsed (4, 2) (-3, 2)

logic pvl = pvl &
      map (uncurry positionElapsed) &
      filter (not . inMiddle) &
      map (id &&& getQuadrant) &
      sortBy (on compare snd) &
      groupBy (on (==) snd) &
      map length &
      product
