# Julia
Pretty name. A language of every good idea three language nerds had made real.

Julia has three "soft" interfaces: Iteration, Indexing, and AbstractArrays (I'm
somewhat wrong about that last one). These "interfaces" are not types, as only
being able to inherit from one ADT at a time prohibits something being all
three, which an Array clearly is. Instead, Julia appears to have a function
called `applicable(f, args...)` which check to see if the function `f(args...)`
exists in Julia's environment.

## Idioms
`key === keys[index] || isequal(key, keys[index])`: Checking if identical in
memory, and then a "softer" check of equality that allows for custom `==`
checks.

## Weaknesses
No ability to type functions. Thus, I can't have an attribute or function
parameter that only accepts a particular type of function. The work-around is
leaving that untyped (or just `::Function`), and doing validation by hand.

The "soft interfaces" mentioned above are inferior to something like Rust's
Traits. I have no way of knowing if I satisfy the requirements of the interface
without careful testing, and I can't state that a variable must satisfy the
interface without trying to use the implied methods and seeing if it explodes.

## Dict
```julia
mutable struct Dict{K,V} <: Associative{K,V}
    # Tracks whether a value exists at this index
    slots::Array{UInt8,1}
    # The stored keys
    keys::Array{K,1}
    # The stored values
    vals::Array{V,1}
    # ???
    ndel::Int
    # How many values are being stored
    count::Int
    # Counts modifications to the table, whether it be resizing, inserting, or
    # deleting.
    age::UInt
    # an index <= the indexes of all used slots
    idxfloor::Int
    # The largest probing run seen. Helps with determining when to stop
    # searching, as well as providing a performance statistic.
    maxprobe::Int
```
Julia implements a hash table using three arrays: one for keys, one for values,
and one for detecting the state of that slot, where state is one of empty,
filled, or missing (were it only two states, a BitArray could be used instead).

Uses linear probing. Interesting.

Automatically resizes if probing takes beyond the maximum allowed probing
situation, which is calculated to be no more than 1/64, or `size >> 6`, of the
table size.

dict.jl:L379: We prepare to enter a loop that will linearly probe for an
unfilled-spot in the hash table. A couple invariants hold at this point:
* `iter == maxprobe && iter < maxallowed`
* `index == (hashindex(key,sz) + maxprobe) & (size-1) + 1`
If _no_ spots are found before we reach our cut-off point, the code resizes the
entire table by a factor of 2 or 4 (if less than 64k elements), and retries the
entire function.

Assuming a completely full table, we would go to a sparsity of 1 in 4 spots
being filled after a 4x growth. Also, assuming the elements were re-hashed and
iid. in the first place, the likelihood of

## Nifty Bit Twiddlin'
Integer modulo: `x & (y-1) + 1 == x % y`
