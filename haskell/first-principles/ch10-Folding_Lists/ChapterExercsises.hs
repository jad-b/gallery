module ChapterExercises where

stops :: String
stops = "pbtdkg"
vowels :: String
vowels = "aeiou"

stopVowelStop :: [String]
stopVowelStop = [a: b:c:[] | a <- stops, b <- vowels, c <- stops]
pSVS :: [String]
pSVS = [x | x <- stopVowelStop, head x == 'p']


-- |The average characters per non-whitespace word in the string
seekritFunc :: Fractional a => String -> a
seekritFunc x =
  (fromIntegral $ sum (map length (words x))) / (fromIntegral $ length (words x))
