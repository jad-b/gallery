import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC

import Numbers

main = defaultMain unitTests

unitTests = testGroup "Tests" [
        numberTests,
        fibTests,
        sigmaTests,
        integralTest
    ]

numberTests = testGroup "numeric" [
    -- Euclid's algorithm for GCD
    testCase "number" $ Numbers.euclids 206 40 @?= 2
  ]

fibTests = testGroup  "fib" [
        testCase "3" $
           Numbers.fib 3   @?=  sum [Numbers.fib (2), Numbers.fib (1)],
        testCase "10" $
           Numbers.fib 10   @?= sum [Numbers.fib (9), Numbers.fib (8)],
        testCase "100" $
           Numbers.fib 100   @?= sum [Numbers.fib (99), Numbers.fib (98)]
    ]

sigmaTests = testGroup "Sigma" [
    testCase "55" $
        Numbers.sigma (\x -> x) [1..10]  @?= 55,
    testCase "Pi Sum" $
        -- 1/(x*(x+2))
        (Numbers.sigma (\x -> (/) 1 $ (*) x $ (+) x 2) [1,5..1000]) * 8 @?= 3.139600623693463
    ]

integralTest = testGroup "Integrals" [
    testCase "cubing" $
        Numbers.integral (\x -> x^3) 0 1 0.01 @?= 0.24998750000000042,
    testCase "fine cubing" $
        Numbers.integral (\x -> x^3) 0 1 0.001 @?= 0.24999987500000073
    ]
