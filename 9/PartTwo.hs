module Main where
import Data.Function
import Data.Functor
import Data.Array.IO
import Data.IORef
import Control.Monad

main = do
  s <- readFile "in.txt"
  let
    p = parse s
  z <- p
  b <- getBounds z
  test z 1 (b & snd)
  e <- getElems z
  print e
  pure ()

te z = do
  writeArray z 0 ("a", 1)
parse :: String -> IO (IOArray Int (String, Int))
parse xs = newListArray (0, length a - 1) a
 where
   i = init xs
   a = i &
     zip [0..] &
     map (\(n, z) -> let z' = read [z] in if odd n then (".", z') else (div n 2 & show, z'))

test arr l r = if (l > r) then pure () else do
  (_, nd) <- readArray arr l
  (z, nz) <- readArray arr r
  let
    zd = nd - nz
    m = max nd zd
  writeArray arr l (z, m)
  writeArray arr r (if m == 0 then "." else z, zd & min 0)
  test arr (l + 2) (r - 2)
