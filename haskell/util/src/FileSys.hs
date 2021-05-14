module FileSys
  ( walk
  , joinT
  , liftMM
  , bindT
  , prependFrontMatter
  ) where

import Control.Monad
import Data.Traversable as T
import System.Directory
import System.FilePath

joinT :: (Monad m, Monad n, Traversable n) => m (n (m (n a))) -> m (n a)
joinT = (>>= liftM join . T.sequence)

liftMM :: (Monad m, Monad n) => (a -> b) -> m (n a) -> m (n b)
liftMM = liftM . liftM

bindT ::
     (Monad m, Traversable t, Monad t) => m (t a) -> (a -> m (t b)) -> m (t b)
bindT x f = joinT (liftMM f x)

-- https://stackoverflow.com/questions/28214913/how-to-use-bind-with-nested-monads
walk :: FilePath -> IO [FilePath]
walk fp = do
  putStr $ "@" ++ fp
  b <- doesDirectoryExist fp
  if b
    then do
      putStrLn "- directory"
      listDirectory fp `bindT` (walk . (</>) fp)
    else do
      putStrLn "- file"
      return [fp]

{-
  Prepend Zola front-matter to the Markdown file
-}
prependFrontMatter :: String -> String
prependFrontMatter s = do
  if (take 3 s == "+++")
    then "+++\n+++\n" ++ s
    else s
