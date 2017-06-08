package algo

// UnionFind can tell you whether two values are connected (Find), or connect
// two separate values (Union).
type UnionFind interface {
	// Find returns whether two values are connected
	Find(p, q int) bool
	// Union connects two values
	Union(p, q int)
}

// QuickFind is a fast-Find, slow-Union implementation
//
// Time complexity: O(N + M)
type QuickFind struct {
	arr []int
}

// NewQuickFind instantiates a UnionFind on the given values. They are assumed
// to be unconnected.
func NewQuickFind(values ...int) UnionFind {
	arr := make([]int, len(values))
	copy(arr, values)
	return &QuickFind{arr: arr}
}

// Find compares the groups of the two values for equality.
//
// Time complexity: O(1)
func (qf *QuickFind) Find(p, q int) bool {
	return qf.arr[p] == qf.arr[q] // O(1) + O(1)
}

// Union connects the two sets of values by re-assigning all members of the
// first group to the second.
//
// Time complexity: O(n)
func (qf *QuickFind) Union(p, q int) {
	pGroup := qf.arr[p] // O(1)
	qGroup := qf.arr[q] // O(1); q's group may != q
	if pGroup == qGroup {
		return // No work to be done
	}
	// Re-assign all members of p's group to q's
	for i, val := range qf.arr { // O(n)
		if val == pGroup { // O(1)
			qf.arr[i] = qGroup // O(1)
		}
	}
	// (O(1) * 3) + O(n) == O(n)
}

// QuickUnion emphasizes Union operations with O(1) speed, but requires
// potentially O(n) to perform a find.
//
// Time complexity: O(N + M)
type QuickUnion struct {
	arr []int
}

// NewQuickUnion instantiates a UnionFind on the given values. They are assumed
// to be unconnected.
func NewQuickUnion(values ...int) UnionFind {
	arr := make([]int, len(values))
	copy(arr, values)
	return &QuickUnion{arr: arr}
}

// Root finds the root value of a component
//
// Time complexity: O(n)
func (qu *QuickUnion) Root(v int) int {
	i := v
	for i != qu.arr[i] {
		i = qu.arr[i]
	}
	return i
}

// Find compares the groups of the two values for equality.
//
// Time complexity: O(n)
func (qu *QuickUnion) Find(p, q int) bool {
	return qu.Root(p) == qu.Root(q)
}

// Union connects the two sets of values by re-assigning all members of the
// first group to the second.
//
// Time complexity: O(n) (Worst case). If you have a completely linear tree,
// and you call Union(leaf, leaf), you will get O(2n) traversals.
func (qu *QuickUnion) Union(p, q int) {
	qu.arr[qu.Root(p)] = qu.Root(q)
}

// WeightedQuickUnion unions the smaller tree to the larger trees during the
// Union operation. It does so by maintaing a separate size array that tracks
// the tree size, e.g. the number of nodes in the tree.
//
// Time complexity: O(N + M log N)
type WeightedQuickUnion struct {
	*QuickUnion
	size []int
}

// NewWeightedQuickUnion instantiates the member array and size array.
func NewWeightedQuickUnion(values ...int) UnionFind {
	arr := make([]int, len(values))
	copy(arr, values)
	return &WeightedQuickUnion{
		QuickUnion: &QuickUnion{
			arr: arr,
		},
		size: make([]int, len(values)),
	}
}

// Union connects the two sets of values by re-assigning all members of the
// first group to the second. It compares the sizes of the members trees,
// linking the smaller tree into the larger.
//
// Time complexity: O(lg n).
// Proof: Given a tree T1 containing a node x, any union causes a depth
// increase of 1 for x, due to its new root in a tree, T2. The total size of
// the tree previously containing, T1, x at least doubles, since |T2| >= |T1|.
// However, it can only double lg n times, since there are only n nodes
// available.
func (wqu *WeightedQuickUnion) Union(p, q int) {
	pRoot := wqu.Root(p)
	qRoot := wqu.Root(q)
	if wqu.size[pRoot] < wqu.size[qRoot] {
		wqu.arr[pRoot] = qRoot
		wqu.size[qRoot] += wqu.size[pRoot]
	} else {
		wqu.arr[qRoot] = pRoot
		wqu.size[pRoot] += wqu.size[qRoot]
	}
}

// WeightedCompressedQuickUnion employs path compression when searching for its
// root.
//
// Time Complexity: O(N + M lg* N); where lg* N is the number of times you'd
// need to take lg N to equal 1.
//
// TODO Does reimplementing Root here override the embedded
// WeightedQuickUnion's Root() method?
type WeightedCompressedQuickUnion struct {
	*QuickUnion
}

// NewWeightedCompressedQuickUnion instantiates the member array and size array.
func NewWeightedCompressedQuickUnion(values ...int) UnionFind {
	arr := make([]int, len(values))
	copy(arr, values)
	return &WeightedCompressedQuickUnion{
		QuickUnion: &QuickUnion{
			arr: arr,
		},
	}
}

// Root returns the root of the tree v is a membmer to. It also compresses
// paths, setting the root of parent's to their grand-parents. This is not as
// thorough as the use of a second loop that roots_all_ visited nodes to the
// found root, but it comes out close enough.
func (wcqu *WeightedCompressedQuickUnion) Root(v int) int {
	i := v
	for i != wcqu.arr[i] { // O(lg n); guaranteed by WeightedQuickUnion
		wcqu.arr[i] = wcqu.arr[wcqu.arr[i]] // O(1) + O(2)
		i = wcqu.arr[i]                     // O(1)
	} // O(lg n + 3 + 1) => O(lg n)
	return i
}
