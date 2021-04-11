module Ch10Exercises where

showFoldFn :: String -> String -> String -> String
showFoldFn fnName x y = concat ["(",fnName," ",x," ",y,")"]

title :: (Show a, Show b) => String -> String -> b -> [a] -> String
title fld fn acc ys = concat [fld," (",fn,") ",show acc," ",show ys]

-- | Prints unevaluated call tree, using the name of the function.
-- Usage:
--   showFolds "const" 'a' [1..5]
showFolds :: (Show a, Show b) => String -> b -> [a] -> IO ()
showFolds fnName b xs =
  let
    f' = showFoldFn fnName
  in
    do
      print $ title "foldr" fnName b xs
      print $ foldr f' (show b) (map show xs)
      print $ title "foldr" ("flip " ++ fnName) b xs
      print $ foldr (flip f') (show b) (map show xs)
      print $ title "foldl" fnName b xs
      print $ foldl f' (show b) (map show xs)
      print $ title "foldl" ("flip " ++ fnName) b xs
      print $ foldl (flip f') (show b) (map show xs)
