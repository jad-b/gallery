module Numbers (
    approx,
    euclids,
    fib,
    sigma,
    integral,
    simpson
) where

-- Simpson's Rule for approximating integrals
simpson :: (Integral a, Fractional b) => (b -> b) -> a -> a -> a -> b
simpson f a b n =
    let h = fromIntegral (b - a) / fromIntegral n
        coef c -- The term coefficient
           | c == 0 = 1
           | c == n = 1
           | even c = 2
           | otherwise = 4
        term k = f $ (fromIntegral a + fromIntegral k * h)
        iter i acc
           | i > n = acc
           | otherwise = iter (i+1) ((coef i * term i):acc)
        series = iter 0 []
    in h/3 * sigma f series

-- Integral calculation
integral :: (Fractional a, Ord a) => (a -> a) -> a -> a -> a -> a
integral f a b dx = (sigma f (accum (a + dx/2))) * dx
    where
    accum x
        | x > b = []
        | otherwise = x:(accum (x + dx))

-- Sigma summations.
-- Map a function to a List and sum the result.
sigma :: Num a =>  (a1 -> a) ->  [a1] -> a
sigma f l = sum (map f l)

-- Euclid's Algorithm for Greatest Common Divisor.
-- So named because Haskell has its own `gcd`.
euclids x 0 = x
euclids x y = euclids y (mod x y)

fibNaive 0 = 1
fibNaive 1 = 1
fibNaive n = (fibNaive n-1) + (fibNaive n-2)

{-A tail-recursive Fibonacci number generator.
  Works by calculating bottom-up to the desired number.  An alternative shown
  in SICP starts a counter at _n_, and runs until the counter equals 0.
-}
fib n = fibTail 0 1 0
    where fibTail i a b
            | i == n = b
            | otherwise = fibTail (i+1) (a+b) a

-- Approximate equality
approx :: (RealFrac a) => a -> a -> a -> Bool
approx tol a b = abs (a - b) <= tol
    where
    abs x
        | x >= 0 = x
        | otherwise = (-x)

