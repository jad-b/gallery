module Ch4Exercises where

awesome :: [String]
awesome = ["Papuchon", "curry", ":)"]
also :: [String]
also = ["Quake", "The Simons"]
allAwesome :: [[ String ]]
allAwesome = [awesome, also]

myAbs :: Integer -> Integer
myAbs x =
  if x < 0 then
    -x
  else
    x

f :: (a,b) -> (c,d) -> ((b,d),(a,c))
f x y = ( (snd x, snd y)
        , (fst x, fst y)
        )


plus :: Num a => a -> a -> a
plus = (+)

addOne :: [a] -> Int
addOne xs = w `plus` 1
  where w = length xs
