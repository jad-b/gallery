module Primes where

divides :: Integral a => a -> a -> Bool
divides d n = rem n d == 0

-- Least Divisor
ld :: Integral a => a -> a
ld n = ldf 2 n

-- Least Divisor From
ldf :: Integral a => a -> a -> a
ldf k n
  | divides k n = k -- Cleanly divides; our answer
  | k^2 > n     = n -- Grown too large; prime
  | otherwise   = ldf (k+1) n -- Add one and try again

prime0 :: Integral a => a -> Bool
prime0 n
  | n < 1 = error "not a positive integer"
  | n == 1 = False
  -- Test if the least divisor is n itself
  | otherwise = ld n == n
