module DatabaseItem where

import Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
  where
    f :: DatabaseItem -> [UTCTime] -> [UTCTime]
    f a b
      | (DbDate utctime) xs = utctime : xs
      | _ xs = xs


filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
  where
    f :: DatabaseItem -> [Integer] -> [Integer]
    f a b
      | (DbNumber int) xs = int : xs
      | _ xs = xs


mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = foldr max startTime
  where
    times = filterDbDate xs
    startTime = head times
    rmng = tail times


sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber


avgDb :: [DatabaseItem] -> Double
avgDb = \xs -> (sumDb xs) / (fromIntegral $ length $ filterDbNumber xs)
