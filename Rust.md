# Rust

Iterators must be passed as mutable references (`&mut`), as iterating is a
mutating operation.

> Cannot move out of borrowed content
You have attempted to take ownership of a borrowed item. Maybe you had  a
reference, and passed it to a method that takes ownership.

