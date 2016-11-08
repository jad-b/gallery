package algo

import "sort"

// SelectionSort finds the minimum element from the sub-array and swaps it into
// place before iterating.
func SelectionSort(arr ...int) []int {
	for i := 0; i < len(arr)-1; i++ { // 1 + c1 * n
		min := i // c2 * n
		for j := i + 1; j < len(arr); j++ {
			if arr[j] < arr[min] {
				min = j
			}
		}
		sort.IntSlice(arr).Swap(min, i)
	}
	return arr
}
