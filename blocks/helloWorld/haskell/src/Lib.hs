module Lib (
  hello
) where

-- Read as: "The function hello is of a type that takes a String as an
-- argument, and returns a String.
hello :: String -> String
-- '++' is the string concatenation operator. In Haskell, the value of a
-- function is the last evaluated expression.
hello t = "Hello, " ++ t ++ "!"
