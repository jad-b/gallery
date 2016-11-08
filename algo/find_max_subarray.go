package algo

import "math"

// FindMaxSubarray finds the maximum contiguous sum within an array.
func FindMaxSubarray(A []int) (lo int, hi int, total int) {
	lo, hi, total = findMaxSubarray(A, 0, len(A)-1)
	return
}

func findMaxSubarray(A []int, lo, hi int) (int, int, int) {
	if lo == hi { // Single element
		return lo, hi, A[lo]
	}
	mid := (lo + hi) >> 1
	leftMin, leftMax, leftSum := findMaxSubarray(A, lo, mid)
	rightMin, rightMax, rightSum := findMaxSubarray(A, mid+1, hi)
	crossMin, crossMax, crossSum := findMaxCrossingSubarray(A, lo, mid, hi)
	if leftSum > rightSum && leftSum > crossSum {
		return leftMin, leftMax, leftSum
	} else if rightSum > leftSum && rightSum > crossSum {
		return rightMin, rightMax, rightSum
	} else {
		return crossMin, crossMax, crossSum
	}
}

func findMaxCrossingSubarray(A []int, lo, mid, hi int) (int, int, int) {
	leftSum := math.MinInt64
	sum := 0
	maxLeft := -1
	for i := mid; i >= lo; i-- {
		sum = sum + A[i]
		if sum > leftSum {
			leftSum = sum
			maxLeft = i
		}
	}
	rightSum := math.MinInt64
	sum = 0 // Reset
	maxRight := -1
	for i := mid + 1; i <= hi; i++ {
		sum = sum + A[i]
		if sum > rightSum {
			rightSum = sum
			maxRight = i
		}
	}
	return maxLeft, maxRight, leftSum + rightSum
}
