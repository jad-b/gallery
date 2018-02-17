import Control.Concurrent
import Control.Monad
import Data.Char
import Data.Maybe
import Data.Set hiding (map)
import System.Environment
import System.IO
import Text.Printf

main = do
    (f, g, n) <- readFiles
    let dict = fromList (lines f) -- Turn dictionary into Set
        work = chunk n (words g) -- Build n-sized chunks of work
    run n dict work

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
-- Divide work into chunks; lists of lists
chunk n xs = (take n xs):(chunk n (drop n xs))

run n dict work = do
    chan <- newChan
    errs <- getChanContents chan -- errors returned from main thread
    mapM_ (forkIO . thread chan dict) (zip [1..n] work)
    wait n errs 0

wait n xs i = when (i < n) $ case xs of
    Nothing : ys -> wait n ys $! i+1 -- ?! means...?
    Just s  : ys -> putStrLn s >> wait n ys i

thread chan dict (me, xs) = do
    mapM_ spellit xs
    writeChan chan Nothing
  where
    spellit w = when (spell dict w) $
        writeChan chan . Just $ printf "Thread %d: %-25s" (me::Int) w

-- Categorize words not found in the dictionary
spell d w = w `notMember` d

readFiles = do
    [s, n] <- getArgs -- Collect CLI args
    f <- readFile "/usr/share/dict/words" -- Load dictionary
    g <- readFile s -- Read file to spellcheck
    return (f, g, read n)
