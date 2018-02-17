-- src=https://wiki.haskell.org/Tutorials/Programming_Haskell/Introduction
import Data.Char
import Data.Maybe
import Data.List
import qualified Data.Map hiding (map)
import Text.Printf

main :: IO () -- IO: Read in the file "grades"
main = do src <- readFile "grades"
          -- Produce a list of (name, grade) tuples
          -- `lines src`: Split grades by newline.
          -- `split. words`: Split lines by word, turn grade into Integer
          let pairs = map (split . words) (lines src)
          -- Insert the (key, value) pairs into a Map
          -- Data.Map.empty: Empty map
          -- `insert k v Map`: Insert k,v into a Map
          let grades = foldr insert Data.Map.empty pairs
          -- Output each student's average
          mapM_ (draw grades) (sort (Data.Map.keys grades))
        where
          -- Insert grades as lists.
          insert (s, g) = Data.Map.insertWith (++) s [g]
          -- Convert the grade to an Integer
          split [name, grade] = (name, read grade)

-- Output student's grades
-- draw :: Integral a => Data.Map.Map k a -> String -> IO ()
draw grades s = printf "%s\t%s\tAverage: %f\n" s (show marks) avg
  where
    -- Lookup student's grades
    marks = Data.Map.findWithDefault (error "No such student") s grades
    -- Compute average
    -- `fromIntegral x :: Double`:
    avg = sum marks / fromIntegral (length marks) :: Double
