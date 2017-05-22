module Funk (
    _map,
    _reverse,
    _tail
) where

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
_tail :: [a] -> [a]
_tail [] = []
_tail (x:[]) = [x]
_tail (x:xs) = _tail xs
