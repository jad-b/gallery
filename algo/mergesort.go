package algo

// MergeSort sorts a list of integers.
//
// Runtime: O(n log n)
func MergeSort(A []int) []int {
	return mergeSort(A, 0, len(A))
}

func mergeSort(A []int, lo, hi int) []int {
	if lo < hi {
		mid := (lo + hi) >> 1
		mergeSort(A, lo, mid)
		mergeSort(A, mid+1, hi)
		merge(A, lo, mid, hi)
	}
	return A
}

func merge(arr []int, lo, mid, hi int) []int {
	var C []int
	return C
}
