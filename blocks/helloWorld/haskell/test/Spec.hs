import Lib (hello)
-- Tasty provides a common testing interface over the popular testing
-- libraries, like quickcheck, smallcheck, and HUnit. These have to be manually
-- added to the .cabal file's test-suite dependencies, afaik.
import Test.Tasty
import Test.Tasty.HUnit

-- Tests, like the main Haskell executable, need to a have a main of type IO
-- (). Tasty's defaultMain :: Tasty.TestTree -> IO ().
main :: IO ()
main = defaultMain unitTests

-- Tasty groups tests hierarchically into trees.
unitTests :: TestTree
unitTests = testGroup "Hello, World: The Unit Test Edition" [
  -- Each testCase is its own TestTree, sort of like how any node in a tree can
  -- be treated as the parent of its own sub-tree.
    testCase "Hello, world!" (assertEqual "Hello World test" "Hello, world!" (hello  "world")),

    -- '$' is syntactic sugar for removing parentheses. Anything following has
    -- a higher precendence in being evaluated than anything that came before.
    -- It kind of adds a parent node between the two expressions, forcing them
    -- to be evaluated separately of one another.
    -- The '@=?' is syntactic sugar for asserting equality, infix-style. This
    -- one compares the expected (LHS) to the actual (RHS);  it's brother, @?=,
    -- does the reverse.
    testCase "Hello, you!" $ "Hello, you!" @=? hello "you"
  ]
