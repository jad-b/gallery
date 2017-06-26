//! Graphs in Rust
//!
//! Man I love graphs. Love 'em. They're like, the data structure's data structure. What else more
//! rawly captures the tangled nature of life? But they're not always great for every operation, so
//! you have to use more _derivative_ forms of data, like tabular, or vectorized.
//!
//! But graphs in Rust have some issues, namely lifetimes and mutability. Mutability's tricky
//! because graphs are recursive data structures, in that Nodes point to more Nodes, and if they
//! didn't, it wouldn't be a very useful graph.
