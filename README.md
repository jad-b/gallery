# Back to School
Collection of algorithms & data structures across a multitude of languages.

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
#### Abstract Data Types
Courtesy of [NIST](https://xlinux.nist.gov/dads/HTML/abstractDataType.html)

* Bag: An unordered collection of values that may have duplicates.
* Dictionary: An abstract data type storing items, accessed by a key.
* Priority Queue: Fast access to the min/max value.
* Queue (FIFO)
* Set: Unordered collection of unique values.
* Stack (LIFO)

#### The Catalog
- [x] Linked List
- [x] Dynamic (resizable) Array
- [x] Ring Buffer
- [x] Hash Set
- [x] Hash Map w/ chaining
- [x] Binary Heap
- [ ] n-ary Tree
- [x] Binary Search Tree
- [ ] Prefix tree
- [ ] Suffix tree
- [ ] Graph: Adjacency List
- [ ] B-Tree
- [ ] LRU Cache
- [ ] Bloom Filter
- [ ] Union-Find

### Algorithms
- [ ] Binary Search
- [ ] Randomized Quick Sort
- [ ] Mergesort
- [ ] BFS
  - [ ] BFS thru matrix
  - [ ] DFS thru matrix
- [ ] DFS w/ cycle detection
- [ ] Topological Sort using Tarjan's Algorithm
- [ ] Dijkstra's Algorithm (w/o decrease-key)
- [ ] Longest Common Subsequence
- [ ] Knapsack

## Thoughts
Given SIMD, could you have a `map` that acts on vectors in parallel? Not only
that, but could you compile the map to use different sized vector operations
depending on the architecture?

Project: Automatic Big-O calculator, using benchmarks

Project: Graph ADTs. Nodes: methods, ADTs. Edges: Has (ADT->method), Using
(method->method)
