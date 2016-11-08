package algo

import (
	"fmt"
	"math"
	"math/rand"
	"testing"
)

func TestFindMaxSubarray(t *testing.T) {
	testCases := []struct {
		arr   []int
		lo    int
		hi    int
		total int
	}{
		{[]int{1, -4, 3, -4}, 2, 2, 3},
		{[]int{13, -3, -25, 20, -3, -16, -23, 18, 20, -7, 12, -5, -22, 15, -4, 7},
			7, 10, 43},
	}
	for _, tc := range testCases {
		t.Run(fmt.Sprintf("total=%d", tc.total), func(t *testing.T) {
			lo, hi, total := FindMaxSubarray(tc.arr)
			if lo != tc.lo || hi != tc.hi || total != tc.total {
				t.Fatalf("\nExp) lo=%d hi=%d sum=%d\nGot) lo=%d hi=%d sum=%d",
					tc.lo, tc.hi, tc.total, lo, hi, total)
			}
		})
	}
}

func BenchmarkFindMaxSubarray(b *testing.B) {
	powersOfTwo := []int{2, 4, 8, 16}
	norm := int(math.Exp2(15.))

	for _, pwr := range powersOfTwo {
		size := int(math.Exp2(float64(pwr)))
		values := make([]int, size)
		for i := 0; i < size; i++ {
			values[i] = rand.Int() - norm
		}
		b.Run(fmt.Sprintf("size=%d", size), func(b *testing.B) {
			for n := 0; n < b.N; n++ {
				FindMaxSubarray(values)
			}
		})
	}
}
