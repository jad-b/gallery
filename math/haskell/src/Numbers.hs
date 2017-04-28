module Numbers (
    euclids,
) where


-- Euclid's Algorithm for Greatest Common Divisor.
-- So named because Haskell has its own `gcd`.
euclids x 0 = x
euclids x y = euclids y (mod x y)
