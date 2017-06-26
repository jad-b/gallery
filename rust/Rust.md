## Overview
### Level 1
Rust is a safety-first statically-typed compiled language. It creates safety
through enforcement of data _ownership_, mutability, and lifetimes. By carefully
tracking ownership and lifetimes, Rust does not need a garbage collector.

## Ownership
### Level 1
Every piece of data in Rust has one, and only one, owner.



### Level 2
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
This, by definition, prevents data races. Example [here](https://is.gd/76weX4)

Interestingly, you can borrow an immutable reference from a mutable reference,
_but_ it renders the mutability of the lender moot. For example:

```rust
let x = 3; // The original
let mut_x = &mut x;
let immut_x = &*mut_x;
println!("x is {}", immut_x);

// But the next line will raise errors, due to the immutable reference having
// "borrowed" away from the mutable one:
// *mut_x += 1;
```

## Lifetimes
A lifetime is the name of a scope within your program. Variables can not outlive
their scope, and thus neither can references to these variables.

## Mutability
### Level 1
Controlling data mutability means Rust tracks who can read and who can modify
data, and when. Many readers of the data can peacefully co-exist, but only one
writer can be allowed at any time. Otherwise, the data could change underneath a
reader's feet.

## Questions
> "...do with Rc and Arc? Well, they both use interior mutability for their
> reference count." How so?

> Will Rust chain `Deref`s until it finds a concrete value?

> `Option.map` vs. `Option.and_then`? Also, why is `Option.and_then` the
> `flatmap` of the Option world?

> What "cost" is missing in "zero-cost" abstractions?
I believe this has to do with Rust getting rid of unnecessary indirection at
compile-time, thus removing pointer-chasing at runtime.

> What are "move semantics"?

> What are traits?
Polymorphism-enablers similar to Haskell's type classes. I believe this to mean,
in a similar manner to Interfaces, you enable sets of operations by giving
something the appropriate Trait.
