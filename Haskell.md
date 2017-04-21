# Haskell
You can make a Haskell library by omitting the `app/` directory and the
executable section within the project's .cabal file. Using a stack template,
like simple-library, could streamline this.

IO actions can _only_ be performed  in the scope of other IO actions. The `main`
function is the highest-level IO action allowed in Haskell. Thus, all IO
actions, if they are to be executed, must somehow fall under `main`. This is why
a library function that reads a file can't have the signature `FilePath ->
String` instead of `FilePath->IO String`. Until linked under `main`, it can only
return the action of reading a String (IO String).
