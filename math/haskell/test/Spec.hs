import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC

import Numbers
import qualified BinaryMobile as BM

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
    testGroup "Sigma Summations" [
        testGroup "SICP vs. Iterators" [
            testCase "Iter: Sum of [1..10]" $ sigma (\x -> x) [1..10]  @?= 55,
            testCase "Iter: Pi Sum" $ do
                -- 1/(x*(x+2))
                let piTerm x = 1 / (x * (x+2))
                sigma piTerm [1,5..1000] * 8 @?= 3.139600623693463,
            testCase "SICP: Sum of [1..10]" $ summate (\x -> x) 1 (+1) 10 @?= 55,
            testCase "SICP: Pi Sum" $ do
                -- 1/(x*(x+2))
                let piTerm x = 1 / (x * (x+2))
                let piNext x = x + 4
                summate piTerm 1 piNext 1000 * 8 @?= 3.139592655589782
        ],
        testGroup "Cubing Integral" [
            testCase "dx=.01" $ assertApproxEqual "Wrong."
                    1.0e-4 0.25 (integral (^3) 0 1 0.01),
            testCase "dx=.001" $ assertApproxEqual "Wrong"
                1.0e-6 0.25 (integral (^3) 0 1 0.001),
            testCase "Simpson's Rule, n=100" $ assertApproxEqual "Not equal"
                    1.0e-12 0.25 (simpson (^3) 0 1 100),
            testCase "Simpson's Rule, n=1000" $ assertApproxEqual "Not equal"
                    1.0e-12 0.25 (simpson (^3) 0 1 1000)
        ]
    ],
    testGroup "Iterative Improvements" [
        testCase "y = sin y + cos y" $ assertApproxEqual "Nope."
            1e-5 1.2587315962971173 (fixedPoint (\y -> sin y + cos y) 1e-5 1.0)
    ],
    testGroup "Continuous Fractions" [
        testCase "Golden Ratio approximator" (do
            let ni = \x -> 1.0
            let di = \x -> 1.0
            let recurAnswer = contFrac ni di 12 "recursive"
            let iterAnswer = contFrac ni di 12 "iterative"
            assertEqual "Iterative equals Recursive" recurAnswer iterAnswer
        ),
        testCase "Euler's natural number approximator" (do
            let ni = \x -> 1.0
            -- Convert Int to Num
            let di = \x -> fromIntegral (eulerExp x)
            let answer = contFrac ni di 11 "iterative"
            let approxE = 2.7182818284590 - 2
            assertApproxEqual "e within 1e-2" (1.0e-2::Double) approxE answer
        )
    ]
 ]

binaryMobileTests = [
    testGroup "Binary Mobile" [
        testCase "How 'bout this" (do
            let bm = BM.BinaryMobile (BM.Branch 7 (BM.Weight 10)) (BM.Branch 7 (BM.Weight 10))
            (BM.length . BM.left) bm @?= 7
        )
    ]
 ]
