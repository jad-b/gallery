import System.Directory

import Test.Tasty
import Test.Tasty.HUnit

import Lib (readInFile)

main :: IO ()
main = defaultMain unitTests

unitTests = testGroup "File I/O" [
    testCase "Hi." $ do -- Remember: '$'  is a way of separating the evaluation of its LHS/RHS
      contents <- readInFile "../testdata/hi.txt"
      contents @?= "hi"
  ]
