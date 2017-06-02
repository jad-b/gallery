module Funk where

_acc_n op init ([]:_) = [] -- First sequence being empty implies all sequences are empty
_acc_n op init seqs = (_acc op init (map head seqs)):(_acc_n op init (map tail seqs))

-- A.K.A. foldr
_acc :: (a -> b -> b) -> b -> [a] -> b
_acc op init [] = init
_acc op init (x:xs) = (op x (_acc op init xs))

-- Map implementation
_map :: (a -> b) -> [a] -> [b]
_map f [] = []
_map f (x:xs) = (f x):(_map f xs)

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
_last (x:xs) = _last xs
