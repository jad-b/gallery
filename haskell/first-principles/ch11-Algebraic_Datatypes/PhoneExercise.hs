{-|
Module: PhoneExercise
Description: "Phone exercise", pg. 450.
 -}
module PhoneExercise where


data Phone = Phone

-- Convert the following conversations into the key presses
-- required to express them.
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

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

-- |Convert a charcter to its digit and the number of presses required.
-- It returns a list, as capitalized characters will include a ('*', 1) tuple.
digitizeChar :: Phone -> Char -> [(Digit, Presses)]
digitizeChar = undefined

-- |
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
