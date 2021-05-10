{-|
Module: PhoneExercise
Description: "Phone exercise", pg. 450.
 -}
module PhoneExercise where

import qualified Data.Map.Strict as M

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

data Phone = Phone (M.Map Char (Digit, Presses))

mkPhone :: Phone
mkPhone =
  let
    t9layout :: [(Digit, String)]
    t9layout =
      [ ('1', "")
      , ('2', "abc")
      , ('3', "def")
      , ('4', "ghi")
      , ('5', "jkl")
      , ('6', "mno")
      , ('7', "pqrs")
      , ('8', "tuv")
      , ('9', "wxyz")
      -- ('*', '^')
      -- ^Uppercases; omitted in lieu of conditional logic
      , ('0', "_")
      , ('#', ".,")
      ]
    charPresses :: M.Map Char (Digit, Presses)
    charPresses = undefined
    -- ^NEXT. I want to fold the t9layout into map, where each digit
    -- can yield multiple entries.
    -- My accum fn. will return a [(Char, (Digit, Presses)]
    -- Example:
    --
    -- > f ('2', "abc") = [('a', ('2', 1)), ('b', ('2', 2)), ('c', ('2', 3))]

-- |Test strings
convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol OK. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "OK. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]

-- |Convert a charcter to its digit and the number of presses required.
-- It returns a list, as capitalized characters will include a ('*', 1) tuple.
digitizeChar :: Phone -> Char -> [(Digit, Presses)]
digitizeChar = undefined

-- |Translates a string into an ordered list of key presses
digitizeStr :: Phone -> String -> [(Digit, Presses)]
digitizeStr = undefined

-- |Total number of presses per digit
cost :: [(Digit, Presses)] -> Presses
cost = foldr ((+) . snd) 0

-- |What is the most popular letter for each message?
-- Defined by frequency
mostPopularLetter :: String -> Char
mostPopularLetter = undefined

-- |Most popular letter overall, defined by frequency
coolestLtr :: [String] -> Char
coolestLtr = undefined

-- |Most popular word, defined by frequency
coolestWord :: [String] -> String
coolestWord = undefined
