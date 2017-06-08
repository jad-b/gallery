package algo

// InversionSort takes a list of inversions and an unsorted array, and sorts in
// O(n) time.
//
// The key is to maintain an indexing array, indicating where the referenced
// variable is currently. Every swap is then preceded by a lookup against the
// indexing array to retrieve the current position.
func InversionSort(A []int, inversions [][2]int) {
	// Prepare index array
	index := make([]int, len(A))
	for i := 0; i < len(A); i++ {
		index[i] = i
	}
	for _, inv := range inversions {
		// Lookup current positions in index
		p, q := index[inv[0]], index[inv[1]]
		A[p], A[q] = A[q], A[p] // Swap!
		// Update the index
		index[q], index[p] = index[p], index[q]
	}
}
