import Control.Monad
import Data.Char
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO hiding (withFile)
import Text.Printf

main :: IO()
main = do
    (args, files) <- getArgs >>= parse
    --
    when (Unbuffered `elem` args) $ hSetBuffering stdout NoBuffering
    mapM_ (cat args) files

cat [] f = withFile f id -- No args ⇒  print file
cat args f = withFile f (newline . number . visible args)
  where
    number s = if Blanks `elem` args then numberSome s else ifset Number numberAll s
    -- Add a dollar sign to the end of each line
    newline s = ifset Dollar (map (++"$")) s
    visible args s = foldl' (flip render) s args
    -- Apply function if flag's set, else use identity function
    ifset a f = if a `elem` args then f else id

# TODO: 2.4
render _ = ()

-- Load file, split lines, f(), join lines, print
withFile filename f = putStr . unlines . f . lines =<< open filename
    where
        open f = if f == "-"
                 then getContents -- stdin
                 else readFile f


data Flag = Blanks -- ^ -b
          | Dollar -- ^ -e
          | Squeeze -- ^ -s
          | Tabs -- ^ -t
          | Unbuffered -- ^ -u
          | Invisible -- ^ -v
          | Number -- ^ -n
          | Help -- ^ --help
          deriving (Eq,Ord,Enum,Show,Bounded)

flags =
    [Option ['b'] []       (NoArg Blanks)
        "Implies the -n option but doesn't count blank lines."
   ,Option ['e'] []       (NoArg Dollar)
        "Implies the -v option and also prints a dollar sign (`$') at the end of each line."
   ,Option ['n'] []       (NoArg Number)
        "Number the output lines, starting at 1."
   ,Option ['s'] []       (NoArg Squeeze)
        "Squeeze multiple adjacent empty lines, causing the output to be single spaced."
   ,Option ['t'] []       (NoArg Tabs)
        "Implies the -v option and also prints tab characters as `^I'."
   ,Option ['u'] []       (NoArg Unbuffered)
        "The output is guaranteed to be unbuffered (see setbuf(3))."
   ,Option ['v'] []       (NoArg Invisible)
        "Displays non-printing characters so they are visible."
   ,Option []    ["help"] (NoArg Help)
        "Print this help message"
   ]

parse argv = case getOpt Permute flags argv of
    (args,fs,[]) -> do -- No error messages
        let files = if null fs then ["-"] else fs -- Default to stdin
        if Help `elem` args
            -- Print help
            then do hPutStrLn stderr (usageInfo header flags)
                    exitWith ExitSuccess
            -- Return
            else return (nub (concatMap set args), files)
    (_,_,errs) -> do
        hPutStrLn stderr (concat errs ++ usageInfo header flags)
        exitWith (ExitFailure 1)
    where header = "Usage: cat [-benstuv] [FILE]..."
          -- Dollar ⇒ Invisible
          set Dollar = [Dollar, Invisible]
          -- Tabs ⇒ Invisible
          set Tabs = [Tabs, Invisible]
          set f = [f]
