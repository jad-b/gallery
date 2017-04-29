import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC

import Numbers

main = defaultMain unitTests

unitTests = testGroup "Tests" [
        numberTests,
        fibTests
    ]

numberTests = testGroup "numeric" [
    -- Euclid's algorithm for GCD
    testCase "number" $ Numbers.euclids 206 40 @?= 2
  ]

fibTests = testGroup  "fib" [
        testCase "3" $
           Numbers.fib(3)  @?=  sum [Numbers.fib (2), Numbers.fib (1)],
        testCase "10" $
           Numbers.fib(10)  @?= sum [Numbers.fib (9), Numbers.fib (8)],
        testCase "100" $
           Numbers.fib(100)  @?= sum [Numbers.fib (99), Numbers.fib (98)]
    ]
