mod robot_grid {

    pub fn main<T>(grid: &Grid<T>) -> &Vec<PathSquare>
        where T: Default + Clone
    {
        unimplemented!();
    }

    pub struct Grid<T: Default + Clone> {
        rows: usize,
        cols: usize,
        grid: Vec<Vec<T>>,
    }

    impl<T: Default + Clone> Grid<T> {
        // Constructor
        fn new(rows: usize, cols: usize) -> Self {
            // Create an immutable 2D grid of vectors, using the default of T
            Self {
                rows: rows,
                cols: cols,
                grid: vec![vec![T::default(); cols]; rows],
            }
        }
        fn right(&self, row: usize, col: usize) -> Option<T> {
            unimplemented!();
        }
        fn down(&self, row: usize, col: usize) -> Option<T> {
            unimplemented!();
        }
        // Return the best move from a square, if there is one.
        fn best_move(&self, row: usize, col: usize) -> Option<&PathSquare> {
            unimplemented!();
        }
    }

    struct PathGrid<T> where T: Default + Clone {
        rows: T,
        cols: T,
        grid: Grid<T>,
    }

    impl<T> PathGrid<T> where T: Default + Clone {
        fn best_path(&self) -> Vec<usize> {
            unimplemented!();
        }
    }

    // PathSquares save the calculated cost & direction at a specific square
    pub struct PathSquare {
        cost: usize,
        direction: Direction,
    }

    // Direction of travel from a square
    enum Direction {
        Right,
        Down
    }

    #[cfg(test)]
    mod tests {
        #[test]
        fn it_works() {
            assert_eq!(2 + 2, 4);
        }
    }
}
