package algo

import (
	"math/rand"
	"testing"
)

func assertSorted(n []int, t *testing.T) (isSorted bool) {
	isSorted = true
	prev := 0
	for i, m := range n {
		if i == 0 { // Skip first value
			prev = m
			continue
		}
		if prev > m {
			isSorted = false
			if t != nil {
				t.Errorf("%d > %d", prev, m)
			}
		}
		prev = m
	}
	return isSorted
}

func TestMergeSort(t *testing.T) {
	input := rand.Perm(100)
	output := MergeSort(input)
	assertSorted(output, t)
}

func benchmarkMergeSort(size int, b *testing.B) {
	for n := 0; n < b.N; n++ {
		MergeSort(rand.Perm(size))
	}
}

func BenchmarkMergeSort10(b *testing.B)     { benchmarkMergeSort(10, b) }
func BenchmarkMergeSort100(b *testing.B)    { benchmarkMergeSort(100, b) }
func BenchmarkMergeSort1000(b *testing.B)   { benchmarkMergeSort(1000, b) }
func BenchmarkMergeSort10000(b *testing.B)  { benchmarkMergeSort(10000, b) }
func BenchmarkMergeSort100000(b *testing.B) { benchmarkMergeSort(100000, b) }
