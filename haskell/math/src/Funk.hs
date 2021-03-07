module Funk where

-- Powerset of a set. 2^n values, so careful what you code for.
powerset :: [a] -> [[a]]
powerset l = pset [] l
  where
    pset x [] = [x]
    pset x xs =
      x : (listcat (map (\(y, ys) -> pset (x ++ [y]) ys) (segment xs)))
          -- Concatenate lists together
    listcat = _acc (++) []
          -- Create a list of (x,xs) pairs
    segment [] = []
    segment (x:xs) = (x, xs) : (segment xs)

_acc_n :: (a -> t -> t) -> t -> [[a]] -> [t]
_acc_n _ _ ([]:_) = [] -- First sequence being empty implies all sequences are empty
_acc_n op init' seqs =
  (_acc op init' (map head seqs)) : (_acc_n op init' (map tail seqs))

-- A.K.A. foldr
_acc :: (a -> b -> b) -> b -> [a] -> b
_acc _ init' [] = init'
_acc op init' (x:xs) = (op x (_acc op init' xs))

-- Map implementation
_map :: (a -> b) -> [a] -> [b]
_map _ [] = []
_map f (x:xs) = (f x) : (_map f xs)

-- SICP Ex.2.18
-- Reverse a list.
_reverse :: [a] -> [a]
_reverse [] = []
_reverse (x:xs) = (_reverse xs) ++ [x]

-- SICP  Ex.2.17
-- Returns the last element of a list
_last :: [a] -> [a]
_last [] = []
_last (x:[]) = [x]
_last (_:xs) = _last xs
