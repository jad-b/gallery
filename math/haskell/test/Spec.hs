import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC

import Numbers

main = defaultMain unitTests

unitTests = testGroup "Tests" numberTests

assertApproxEqual :: (RealFrac a, Show a) => String -> a -> a -> a -> Assertion
assertApproxEqual preface tol expected actual =
  if (approx tol expected actual) then assert () else (assertFailure msg)
  where msg = (if null preface then "" else preface ++ "\n") ++
              "expected: " ++ show expected ++ "\n but got: " ++ show actual

numberTests = [
    testGroup "Approximate equality" $ [
        testCase "Close enough" $
            assertBool "3.49 ~= 3.50" $ approx 0.01 3.49 3.50,
        testCase "Equal" $
            assertBool "3.50 == 3.50" $ approx 0.01 3.50 3.50,
        testCase "Too far" $
            assertBool "3.4 !~= 3.50" $ not (approx 0.01 3.4 3.50)
    ],
    testCase "Euclid's GCD" $ euclids 206 40 @?= 2,
    testGroup "Fibonacci" [
        testCase "3" $ fib 3 @?= fib 2 + fib 1,
        testCase "10" $ fib 10 @?= fib 9 + fib 8,
        testCase "100" $ fib 100 @?= fib 99 + fib 98
    ],
    testGroup "Sigma Summation" [
        testCase "Sum of [1..10]" $ sigma (\x -> x) [1..10]  @?= 55,
        testCase "Pi Sum" $
            -- 1/(x*(x+2))
            (sigma (\x -> (/) 1 $ (*) x $ (+) x 2) [1,5..1000]) * 8 @?= 3.139600623693463,
        testGroup "Cubing Integral" [
            testCase "dx=.01" $ assertApproxEqual "Wrong."
                    1.0e-4 0.25 (integral (^3) 0 1 0.01),
            testCase "dx=.001" $ assertApproxEqual "Wrong"
                1.0e-6 0.25 (integral (^3) 0 1 0.001),
            testCase "Simpson's Rule, n=100" $ assertApproxEqual "Not equal"
                    1.0e-4 0.25 (simpson (^3) 0 1 100),
            testCase "Simpson's Rule, n=1000" $ assertApproxEqual "Not equal"
                    1.0e-6 0.25 (simpson (^3) 0 1 1000)
            ]
        ]
    ]
