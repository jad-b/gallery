# Back to School
I've decided to kill four birds with one stone by re-learning computer science
fundamentals _and_ learning Rust, Julia, and Haskell in one go. To this end, I
will implement each data structure, algorithm, or demonstration of a general
concept in all three langugages. Hopefully, there will be synergy between the
reinforcement of repetitive re-implementation and the modeling across languages.

## Backlog
### Requisite Operations
- [x] Hello World
    - [x] Unit Testing
- [x] File I/O
  - [ ] JSON
- [ ] Benchmarking

### Functional Paradigms
- map: Apply a function to each element in an iterable.
- reduce: Accumulate a list into a scalar.
- flatten: Join arbitrarily nested arrays into a single array. Think of getting
  the leaves from a tree.
- collapse: Like flatten, but with a reduction at each level to one final.
- level: Like flatten, but handles scalars mixed in amongst the lists.
- merge: Given _n_ lists, emit lists of size _n_, with one value from each
  passed list.
- min: For each item, emit the smallest value seen up to that item's index.
- max: Inverse of min.
- sample: Emit values according to a selector function.

### Data Structures
- [ ] Linked List
- [ ] Dynamic Array
- [ ] Hash Set
- [ ] Hash Map w/ chaining
- [ ] Binary Heap
- [ ] Binary Tree
- [ ] n-ary Tree
- [ ] Prefix tree
- [ ] Suffix tree
- [ ] AVL Tree
- [ ] Graph: Adjacency List
- [ ] LRU Cache
#### Bonus
- [ ] B-Tree
- [ ] Bloom Filter
- [ ] Ring Buffer

### Algorithms
- [ ] Binary Search
- [ ] Randomized Quick Sort
- [ ] Mergesort
- [ ] BFS
  - [ ] BFS thru matrix
  - [ ] DFS thru matrix
- [ ] DFS w/ cycle detection
- [ ] Tree traversals
- [ ] Topological Sort using Tarjan's Algo
- [ ] Dijkstra's Algo (w/o decrease-key)
- [ ] Longest Common Subsequence
- [ ] Knapsack

## Thoughts
Given SIMD, could you have a `map` that acts on vectors in parallel? Not only
that, but could you compile the map to use different sized vector operations
depending on the architecture?
