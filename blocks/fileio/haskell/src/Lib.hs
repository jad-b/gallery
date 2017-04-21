module Lib (
  readInFile
) where

import System.IO

{- readInFile is a tiny wrapper around System.IO.readFile, for the purposes of
 - demonstrating how file I/O can be turned into a library function. Note that
 - the return value is _still_ IO String, as we can only escape the IO type
 - (monad? To be learned.) when we're under the authority of the main function,
 - which Haskell allows to perform IO actions.
 -}
readInFile :: FilePath -> IO String
readInFile fp = readFile fp
