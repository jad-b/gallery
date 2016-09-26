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
func (qf *QuickFind) Find(p, q int) bool {
	return qf.arr[p] == qf.arr[q] // O(1) + O(1)
}

// Union connects the two sets of values by re-assigning all members of the
// first group to the second.
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
	// (O(1) * 3) + O(n)
}
