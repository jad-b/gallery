package util

import (
	"fmt"
	"reflect"
	"testing"
)

func TestSeries(t *testing.T) {
	testCases := []struct {
		n   int
		exp []int
	}{
		{5, []int{2, 1, 1, 1}},
		{7, []int{3, 2, 1, 1}},
		{20, []int{10, 6, 4}},
	}
	for _, tc := range testCases {
		t.Run(fmt.Sprintf("%d => %v", tc.n, tc.exp), func(t *testing.T) {
			if !reflect.DeepEqual(Series(tc.n), tc.exp) {
				t.Error("Obs != Exp")
			}
		})
	}
}
