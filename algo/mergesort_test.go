package algo

import (
	"fmt"
	"math"
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
	inputSize := []int{8, 32, 64, 128}
	for _, size := range inputSize {
		t.Run(fmt.Sprintf("size=%d", size), func(t *testing.T) {
			input := rand.Perm(size)
			t.Logf("Input: %v", input)
			output := MergeSort(input)
			assertSorted(output, t)
			t.Logf("Output: %v", output)
		})
	}
}

func BenchmarkMergeSort(b *testing.B) {
	powersOfTwo := []int{1, 2, 4, 8, 16, 32}
	for _, pwrOfTwo := range powersOfTwo {
		input := rand.Perm(int(math.Exp2(float64(pwrOfTwo))))
		b.Run(fmt.Sprintf("2^%d", pwrOfTwo), func(b *testing.B) {
			for n := 0; n < b.N; n++ {
				MergeSort(input)
			}
		})
	}
}
