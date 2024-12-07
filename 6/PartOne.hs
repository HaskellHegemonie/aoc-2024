{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
module Main where
import Data.Array
import Data.Set qualified as S

data Direction = U | D | L | R deriving Show

redirect = \case
  U → R
  R → D
  D → L
  L → U

main ∷ IO ()
main = do
  -- s <- readFile "in.txt"
  s <- readFile "realinput.txt"
  let
    p = parse s
    (po, z) = head $ filter (\(x, y) -> elem y "<>v^") $ assocs p
    d = case z of
      '>' -> R
      '<' -> L
      '^' -> U
      'v' -> D
  print $ logic p S.empty po d

n = negate
parse ∷ String -> Array (Int, Int) Char
parse xs = listArray ((0, 0), (length l + n 1, n 1 + (length $ head l))) $ mconcat l
 where
   l = lines xs

update (x, y) = \case
  U → (x - 1, y)
  D → (x + 1, y)
  R → (x, y + 1)
  L → (x, y - 1)

isValid arr (x', y') = all (uncurry inRange) [((0, mx), x'), ((0, my), y')]
 where
   (x, (mx, my)) = bounds arr

logic arr s p d = if isValid arr p
  then
        logic arr newSet next nextD
  else
  length s
  where
    next' = update p d
    cond = if isValid arr next' then '#' == arr ! next' else False
    next = if cond then update p r else next'
    nextD = if cond then r else d
    r = redirect d
    newSet = S.insert p s
