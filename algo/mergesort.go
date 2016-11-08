package algo

import "math"

// MergeSort sorts a list of integers.
//
// Approximate runtime: O(n log n)
// Exact runtime: 6n(lg n + 1) = 6n*lg n + 6n
func MergeSort(A []int) []int {
	mergeSort(A, 0, len(A)-1)
	return A
}

// mergeSort recursively calls mergeSort on the two halves of the input array,
// then merges their results for the output.
//
// For an input of size n, we will build a recursion tree of It will have a
// depth lg n + 1, accounting for the exponential division of the input array,
// and a constant of one for the base level 0. Each level _j_ of the
// tree will contain 2^j subproblems (recursions), each of which has an
// input of size n/(2^j); thus, the total number of operations for any level of
// the tree is found through 2^j * n/(2^j).
//
// For a given level's call of mergeSort, we can pretend the recursive calls
// are free, as they will be paid for in the sub-level. That only really leaves
// us with the cost of merge(), which we know to be at most 6n. Thus, the
// cost/runtime/time complexity of _any_ given level j in the recursion tree is
// O(merge(n/(2^j) * # of subproblems at j) = 6*(n/2^j) * 2^j = 6n. The total
// cost of mergeSort is then found by multiplying that equation by the # of
// levels in the tree, (lg n + 1), for 6n(lg n + 1), or 6n*lg n + 6n.
func mergeSort(A []int, lo, hi int) {
	if lo < hi {
		mid := (lo + hi) / 2
		mergeSort(A, lo, mid)
		mergeSort(A, mid+1, hi)
		merge(A, lo, mid, hi)
	}
}

// merge combines two arrays
//
// Runtime: 6n
func merge(A []int, lo, mid, hi int) {
	// Copy out each subarray
	lSize, rSize := mid-lo+1, hi-mid
	L := make([]int, lSize+1)
	copy(L, A[lo:mid+1])
	R := make([]int, rSize+1)
	copy(R, A[mid+1:hi+1])
	// Add a sentinel value to the end
	L[lSize] = math.MaxInt64
	R[rSize] = math.MaxInt64

	i, j := 0, 0
	for k := lo; k <= hi; k++ {
		if L[i] <= R[j] {
			A[k] = L[i]
			i++
		} else {
			A[k] = R[j]
			j++
		}
	}
}
