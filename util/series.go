package util

import "math"

// Series yeah!
func Series(n int) []int {
	i := 2
	rem := n
	var seq []int
	for rem > 0 {
		x := math.Max(float64(n/i), 1.0)
		seq = append(seq, int(math.Min(x, float64(rem))))
		rem = rem - int(x)
		i = i + 1
	}
	return seq
}
