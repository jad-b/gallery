module Numbers (
    euclids,
    fib
) where


-- Euclid's Algorithm for Greatest Common Divisor.
-- So named because Haskell has its own `gcd`.
euclids x 0 = x
euclids x y = euclids y (mod x y)

fibNaive 0 = 1
fibNaive 1 = 1
fibNaive n = (fibNaive n-1) + (fibNaive n-2)

fib n = fibTail 0 1 0
    where fibTail i a b
                  | i == n = a + b
                  | otherwise = fibTail (i+1) (a+b) a
