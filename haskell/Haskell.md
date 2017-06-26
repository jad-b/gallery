# Haskell
You can make a Haskell library by omitting the `app/` directory and the
executable section within the project's .cabal file. Using a stack template,
like simple-library, could streamline this.

Haskell supports __parametric__ polymorphism, where the use of _generic types_
allow for applying the same procedures (read: procedures) to different concrete
types.

When exporting new types, you need to list which data constructors you're
exporting as well. See BinaryMobile.hs for details. On the other hand, you can
use `NewType(..)` to export everything associated with the type.

## Monads

### Level 1
Monads provide a means of sequencing actions. This is difficult to do otherwise
using only the functions-as-arguments style, or at least, becomes far less
readable.

### Level 2

### Level 3
IO actions can _only_ be performed  in the scope of other IO actions. The `main`
function is the highest-level IO action allowed in Haskell. Thus, all IO
actions, if they are to be executed, must somehow fall under `main`. This is why
a library function that reads a file can't have the signature `FilePath ->
String` instead of `FilePath->IO String`. Until linked under `main`, it can only
return the action of reading a String (IO String).
