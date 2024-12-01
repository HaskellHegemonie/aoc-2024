module Aux where

newtype Sum a = Sum { getSum :: a }

instance Num a => Semigroup (Sum a) where
  Sum a <> Sum b = Sum $ a + b

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend = (<>)

(f &&& g) x = (f x, g x)

transpose [] = []
transpose ([]:_) = []
transpose ((h:t):xs) = (h : map head xs) : transpose (t : map tail xs)

group [] = []
group [x] = [[x]]
group (x:y:xs) = if x == y then (x : ng) : ngs else [x] : g
  where
    g@(ng:ngs) = group (y:xs)
