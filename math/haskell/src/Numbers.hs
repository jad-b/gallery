module Numbers (
    approx,
    euclids,
    fib,
    sigma,
    summate,
    integral,
    simpson,
    fixedPoint,
    iterimprov
) where

fixedPoint f tol guess = (iterimprov closeEnough f) guess
    where
    closeEnough g = abs (g - (f g)) < tol

-- Iterative improvement of approximate numeric calculations
iterimprov :: (Show a) => (a -> Bool) -> (a -> a) -> (a -> a)
iterimprov arbiter improver = again
    where
    again guess
      | arbiter guess =  guess
      | otherwise =  again (improver guess)

-- Simpson's Rule for approximating integrals.
simpson :: (Integral a, Fractional b) => (b -> b) -> a -> a -> a -> b
simpson f a b n = (h/3) * sigma term [0..n]
    where h = fromIntegral (b - a) / fromIntegral n
          coef k -- The term coefficient
             | k == 0 = 1
             | k == n = 1
             | even k = 2
             | otherwise = 4
          term k = coef k * f (fromIntegral a + (fromIntegral k) * h)

-- Integral calculation
integral :: (Fractional a, Ord a) => (a -> a) -> a -> a -> a -> a
integral f a b dx = (sigma f (accum (a + dx/2))) * dx
    where
    accum x
        | x > b = []
        | otherwise = x:(accum (x + dx))

-- Sigma summation, from SICP
summate term a next b = iter a 0
    where iter a result
             | a > b = result
             | otherwise = iter (next a) (term a + result)

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

