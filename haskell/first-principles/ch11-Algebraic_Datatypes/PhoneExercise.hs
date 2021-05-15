{-|
Module: PhoneExercise
Description: "Phone exercise", pg. 450.
 -}
module PhoneExercise where

import qualified Data.HashMap.Strict as M

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

newtype Phone
  = Phone (M.HashMap Char (Digit, Presses))
    deriving (Show)

mkPhone :: Phone
mkPhone = Phone charPresses
  where
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
      , ('0', "_")
      , ('#', ".,")
      ]
    charPresses :: M.HashMap Char (Digit, Presses)
    charPresses =
      let
        -- |Reverses the Digit -> [Char] mapping to map each Char to the Digit
        -- & number of required presses.
        g :: (Digit, String) -> [(Char, (Digit, Presses))]
        g (d, s) = foldr h [] ys
          where
            h :: (Char, Presses)
              -> [(Char, (Digit, Presses))]
              -> [(Char, (Digit, Presses))]
            h (c, p) zs = (c, (d, p)) : zs
            ys = zipWith (,) s [1..length s]
        -- The next two functions fold the Phone keypad into the reverse map of
        f :: (Digit, String) -> [(Char, (Digit, Presses))] -> [(Char, (Digit, Presses))]
        f = (++) . g
        xs :: [(Char, (Digit, Presses))]
        xs = foldr f [] t9layout
      in
        M.fromList xs

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
digitizeChar (Phone _map) ch = M.lookupDefault [] ch _map

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
