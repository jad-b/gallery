package algo

// UnionFind can tell you whether two values are connected (Find), or connect
// two separate values (Union).
type UnionFind interface {
    // Find returns whether two values are connected
    Find(p, q int) bool
    // Union connects two values
    Union(p, q int)
}

// WCUnionFind (Weighted (Path-)Compressed Union Find) employs path compression
// when searching for its root.
//
// Time Complexity: O(N + M lg* N); where lg* N is the number of times you'd
// need to take lg N to equal 1.
type WCUnionFind struct {
    arr []int
    size []int
}

// NewWeightedCompressedQuickUnion instantiates the member array and size array.
func NewWCUnionFind(values ...int) UnionFind {
    arr := make([]int, len(values))
    copy(arr, values)
    return &WCUnionFind{
        arr: arr,
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
func (wcuf *WCUnionFind) Union(p, q int) {
    pRoot := wqu.Root(p)
    qRoot := wqu.Root(q)
    if pRoot == qRoot { return }
    if wqu.size[pRoot] < wqu.size[qRoot] {
        wqu.arr[pRoot] = qRoot
        wqu.size[qRoot] += wqu.size[pRoot]
    } else {
        wqu.arr[qRoot] = pRoot
        wqu.size[pRoot] += wqu.size[qRoot]
    }
}

// Find compares the groups of the two values for equality.
// Time complexity: O(n)
func (wcuf *WCUnionFind) Find(p, q int) bool {
    return qu.Root(p) == qu.Root(q)
}

// Root returns the root of the tree v is a member to. It also compresses paths,
// setting the root of parent's to their grand-parents. This is not as thorough
// as the use of a second loop that roots_all_ visited nodes to the found root,
// but it comes out close enough.
func (wcuf *WCUnionFind) Root(v int) int {
    i := v
    for i != wcqu.arr[i] { // O(lg n); guaranteed by WeightedQuickUnion
        wcqu.arr[i] = wcqu.arr[wcqu.arr[i]] // O(1) + O(2)
        i = wcqu.arr[i]                     // O(1)
    } // O(lg n + 3 + 1) => O(lg n)
    return i
}
