package algo

import (
	"math/rand"
	"reflect"
	"testing"
)

func setupTestUnionFind(factory func(...int) UnionFind) (UnionFind, []int, [][]int) {
	testCase := []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
	uf := factory(testCase...)
	uf.Union(1, 2)
	uf.Union(3, 4)
	uf.Union(5, 6)
	uf.Union(7, 8)
	uf.Union(7, 9)
	uf.Union(2, 8)
	uf.Union(0, 5)
	return uf, testCase, [][]int{
		[]int{0, 5, 6},
		[]int{1, 2, 7, 8, 9},
		[]int{3, 4},
	}
}

func validateExpectedEqualsObserved(uf UnionFind, testCase []int, exp [][]int, t *testing.T) {
	for _, set := range exp {
		t.Logf("Expected: %v", set)
		for _, val := range set {
			var obsSet []int
			for _, n := range testCase {
				if uf.Find(val, n) {
					t.Logf("%d found in %d", val, n)
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

func TestQuickFind(t *testing.T) {
	t.Parallel()
	// Assert that by iterating over the values and calling Find(), we arrive
	// at our expected groups
	uf, testCase, expGroups := setupTestUnionFind(NewQuickFind)
	validateExpectedEqualsObserved(uf, testCase, expGroups, t)
}

func TestQuickUnion(t *testing.T) {
	t.Parallel()
	uf, testCase, expGroups := setupTestUnionFind(NewQuickUnion)
	validateExpectedEqualsObserved(uf, testCase, expGroups, t)
}

func TestWeightedQuickUnion(t *testing.T) {
	t.Parallel()
	uf, testCase, expGroups := setupTestUnionFind(NewWeightedQuickUnion)
	validateExpectedEqualsObserved(uf, testCase, expGroups, t)
}

func benchmarkQuickUnion(fn func(...int) UnionFind, size int, b *testing.B) {
	for n := 0; n < b.N; n++ {
		uf := fn(intRange(size)...) // Initialize UnionFind w/ values
		operateUnionFind(uf, size)
	}
}

// operateUnionFind executes a set number of random Union operations, followed
// by an equal number of Finds. It does *not* test for correctness.
func operateUnionFind(uf UnionFind, n int) {
	mid := n >> 1
	for i := 0; i < mid; i++ { // Union
		uf.Union(rand.Intn(n), rand.Intn(n))
	}
	for i := 0; i < mid; i++ { // Find
		uf.Find(rand.Intn(n), rand.Intn(n))
	}
}

func intRange(n int) []int {
	ints := make([]int, n)
	for i := 0; i < n; i++ {
		ints[i] = i
	}
	return ints
}

func BenchmarkQuickUnion10(b *testing.B)   { benchmarkQuickUnion(NewQuickUnion, 10, b) }
func BenchmarkQuickUnion100(b *testing.B)  { benchmarkQuickUnion(NewQuickUnion, 100, b) }
func BenchmarkQuickUnion1000(b *testing.B) { benchmarkQuickUnion(NewQuickUnion, 1000, b) }
func BenchmarkWeightedCompressedQuickUnion10(b *testing.B) {
	benchmarkWeightedCompressedQuickUnion(NewWeightedCompressedQuickUnion, 10, b)
}
func BenchmarkWeightedCompressedQuickUnion100(b *testing.B) {
	benchmarkWeightedCompressedQuickUnion(NewWeightedCompressedQuickUnion, 100, b)
}
func BenchmarkWeightedCompressedQuickUnion1000(b *testing.B) {
	benchmarkWeightedCompressedQuickUnion(NewWeightedCompressedQuickUnion, 1000, b)
}
