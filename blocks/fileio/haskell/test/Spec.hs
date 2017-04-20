import System.Directory

import Test.Tasty
import Test.Tasty.HUnit

import Lib (readInFile)


main :: IO ()
main = defaultMain unitTests

unitTests = testGroup "File I/O" [
    testCase "Hi." $ do
      cwd <- getCurrentDirectory
      putStrLn "CWD=" ++ cwd
      contents <- readInFile "../../testdata/hi.txt"
      contents @?= "hi"
  ]
