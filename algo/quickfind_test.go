package algo

import (
	"reflect"
	"testing"
)

func TestQuickFind(t *testing.T) {
	testCase := []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
	uf := NewQuickFind(testCase...)
	uf.Union(1, 2)
	uf.Union(3, 4)
	uf.Union(5, 6)
	uf.Union(7, 8)
	uf.Union(7, 9)
	uf.Union(2, 8)
	uf.Union(0, 5)
	expGroups := [][]int{
		[]int{0, 5, 6},
		[]int{1, 2, 7, 8, 9},
		[]int{3, 4},
	}
	// Assert that by iterating over the values and calling Find(), we arrive
	// at our expected groups
	for _, set := range expGroups {
		for _, val := range set {
			var obsSet []int
			for _, n := range testCase {
				if uf.Find(val, n) {
					obsSet = append(obsSet, n)
				}
			}
			// Compare sets for equality
			if !reflect.DeepEqual(obsSet, set) {
				t.Errorf("%v != %v", obsSet, set)
			}
		}
	}
}
