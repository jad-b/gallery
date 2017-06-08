> What "cost" is missing in "zero-cost" abstractions?

> What are "move semantics"?

> What are traits?
Polymorphism-enablers similar to Haskell's type classes. I believe this to mean,
in a similar manner to Interfaces, you enable sets of operations by giving
something the appropriate Trait.

> Why is stack access faster than the heap? Locality?

## Overview
Rust is a statically-typed compiled language that emphasizes saftey. It is
unique for its notions of data _ownership_ and  behavior polymorphism via
_traits_. Leveraging data ownership allows Rust to automate deallocation and
destruction w/o the need for a garbage collector.

Safety begins by controlling data mutability. Rust emphasizes this by
upgrading the typical variable _assignment_ to variable _binding_: `let x = 5;`.
`x` can not be reassigned, unless we either 1) re-declare `x` entirely, as in
`let x = 10;`, or 2) declare `x` to be mutable from the get-go using `mut`: `let
mut x = 5;`.

Ownership is enforced in Rust by only allowing one pointer at a time to a given
value (memory segment). Primitive types, such as `bool` and `int32` get around
this by having their value copied on assignment. In `let x = 5; let y = x`, both
`x` and `y` have their own bits within the stack. If `x` was of type `Vector`,
`x` would have its ownership moved to `y`, and accessing `x` would cause an
error. Importantly, passing `x` as a function argument _also_ results in a shift
in ownership. To allow a function to modify its arguments and then return
ownership to the caller, the function must return its arguments, and the caller
must re-bind them. However, we can avoid these tedious steps by stating our
intent to "lend" the function our variables through a concept called
"borrowing."

"Borrowing" is the passing of temporary ownership of variables from callee to
caller. It could also be thought of as "leasing" the variables. It uses what is
commonly recognized as the "address of" operator, `&`, providing what Rust calls
a _reference_. While leased, the caller can not access their borrowed variables.
However, when the callee exits scope, the caller has an implied return of
owernship of its borrowed variables. `&` by itself implies a read-only
borrowing, while `&mut` allows for modification. This extends as far as `&`
disallowing operations like `Vector.push`, which would modify the Vector's data.
Thus, the immutability guarantee for a pointer-based object like an array is
much stronger in Rust than in Python, where the immutability only applies to the
pointer itself, e.g. `a = ([],); a[0] = {}` isn't allowed, but `a = ([],);
a[0].append("modify!")` is.

Borrows are scoped to be no greater than the scope of the original owner.
Second, you may have many `&` references, __or__ you can have _one_ `&mut`
reference. In English: You can have many readers, or you can have one writer.
This, by definition, prevents data races.
