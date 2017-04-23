import Test.Tasty
import Test.Tasty.HUnit

import Lib (euclids)

main = defaultMain unitTests

unitTests = testGroup "GCD" [
    testCase "GCD" $ Lib.euclids 206 40 @?= 2
  ]
