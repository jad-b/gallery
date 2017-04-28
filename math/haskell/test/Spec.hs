import Test.Tasty
import Test.Tasty.HUnit

import Numbers

main = defaultMain unitTests


unitTests = testGroup "Tests" [
        gcdTests
    ]

gcdTests = testGroup "GCD" [
    testCase "GCD" $ Numbers.euclids 206 40 @?= 2
  ]
