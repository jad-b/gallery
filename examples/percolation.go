package examples

/*
http://coursera.cs.princeton.edu/algs4/assignments/percolation.html
*/

// Percolation defines the API for testing if a N-by-N grid "percolates"; there is a path of open
// sites from the top to the bottom. All sites begin closed, and are opened at random.
type Percolation interface {
	Open(i, j int) error
	IsOpen(i, j int) (bool, error)
	IsFull(i, j int) (bool, error)
	Percolates() bool
}

// PercolationStats defines the API for measuring the fraction of open sites before percolation is
// achieved.
type PercolationStats interface {
	// Perform trials independent experiments on an N-by-N grid; will need to use Percolation above.
	Test(n, trials int)
	Mean() float64
	StdDev() float64
	ConfidenceLo() float64
	ConfidenceHi() float64
}
