package main

import (
	"flag"
	"fmt"

	"github.com/jad-b/backtoschool/util"
)

func main() {
	n := flag.Int("n", 1, "Number to divide")
	flag.Parse()

	seq := util.Series(*n)
	for _, x := range seq {
		fmt.Printf("%d ", x)
	}
	fmt.Println()
}
