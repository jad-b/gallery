import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC

import Numbers

main = defaultMain unitTests


unitTests = testGroup "Tests" [
        numberTests
    ]

numberTests = testGroup "numeric" [
    -- Euclid's algorithm for GCD
    testCase "number" $ Numbers.euclids 206 40 @?= 2 ,
    -- Fibonacci
    SC.testProperty "fibonacci" $
        \x -> Numbers.fib x == sum Numbers.fib (x-2) Numbers.fib (x-1)
  ]
