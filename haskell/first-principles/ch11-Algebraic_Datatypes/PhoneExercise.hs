{-|
Module: PhoneExercise
Description: "Phone exercise", pg. 450.
 -}
module PhoneExercise where

import Data.Char (isUpper, toLower)
import qualified Data.HashMap.Strict as M

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

newtype Phone
  = Phone (M.HashMap Char (Digit, Presses))
    deriving (Show)

mkPhone :: Phone
mkPhone = Phone charToDigitPress
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
      , ('0', "+ ")
      , ('#', ".,")
      ]
    -- |Reverses the Digit -> [Char] mapping to map each Char to the Digit
    -- & number of required presses.
    charToDigitPress :: M.HashMap Char (Digit, Presses)
    charToDigitPress =
      let
        -- Expand each Digit -> Symbols tuple into a reverse mapping of the
        -- individual characters -> (Digit, # of Presses)
        revDigitMap :: (Digit, String) -> [(Char, (Digit, Presses))]
        revDigitMap (d, s) = foldr (\(c, p) zs -> (c, (d, p)) : zs) [] ys
          where
            ys = zipWith (,) s [1..length s]
        letterPresses :: [(Char, (Digit, Presses))]
        letterPresses = foldr ((++) . revDigitMap) [] t9layout
        -- The digit is obtained by pressing once more than its # of letters
        digitPresses :: [(Char, (Digit, Presses))]
        digitPresses = foldr ((:) . \(d, s) -> (d, (d, length s + 1))) [] t9layout
      in
        M.union (M.fromList letterPresses) (M.fromList digitPresses)

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
digitizeChar (Phone _map) ch
    | isUpper ch =  M.lookupDefault ('_', 0) (toLower ch) _map : ('*', 1) : []
    | otherwise = M.lookupDefault ('_', 0) ch _map : []

-- |Translates a string into an ordered list of key presses
digitizeStr :: Phone -> String -> [(Digit, Presses)]
digitizeStr p s = foldr ((++) . (\c -> digitizeChar p c)) [] s

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
