import System.Environment
import System.Exit

main = getArgs >>= parse >>= putStr . tac

-- (String -> [String]) -> ([a] -> [a]) -> ([String] -> String)
tac = unlines . reverse . lines

parse ["-h"] = usage >> exit
parse ["-v"] = version >> exit
parse [] = getContents
-- Read the contents from a list of filepaths, then join them
parse files = fmap concat (mapM readFile files)

usage = putStrLn "Usage: tac [-vh] [FILE]..."
version = putStrLn "Haskell tac 0.1"
exit = exitWith ExitSuccess
die = exitWith (ExitFailure 1)

# 2.3 GetOpt
