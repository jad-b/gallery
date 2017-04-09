import Lib (hello)
import Test.HUnit

main = TestCase (assertEqual "Hey world." "Hello, world!" (hello  "world"))
