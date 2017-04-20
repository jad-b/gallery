module Lib (
  readInFile
) where

import System.IO

{-readInFile :: FilePath -> String-}
readInFile fp = do
  contents <- readFile fp
  return contents
