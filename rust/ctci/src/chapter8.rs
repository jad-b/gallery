pub struct Grid<T: Default + Clone> {
    rows: usize,
    cols: usize,
    grid: Vec<Vec<T>>,
}

impl<T: Default + Clone> Grid<T> {
    // Constructor
    pub fn new(rows: usize, cols: usize) -> Self {
        // Create an immutable 2D grid of vectors, using the default of T
        Self {
            rows: rows,
            cols: cols,
            grid: vec![vec![T::default(); cols]; rows],
        }
    }
}

pub mod robot_grid {
    // super refers to the parent module
    use super::Grid;
    // Enums provide a namespace
    use self::Direction::{Nowhere,Right,Down};

    pub fn solve(grid: Grid<bool>) -> Vec<Direction> {
        GridMap::new(grid).best_path()
    }

    // Extend the Grid implementation
    impl<T: Default + Clone> Grid<T> {
        // Return the cost of going right
        fn right(&self, row: usize, col: usize) -> Option<T> {
            unimplemented!();
        }
        // Return the cost of going down
        fn down(&self, row: usize, col: usize) -> Option<T> {
            unimplemented!();
        }
        // Return the best move from a square, if there is one.
        fn best_move(&self, row: usize, col: usize) -> Option<&PathSquare> {
            unimplemented!();
        }
    }

    /// A map of paths through a given grid.
    struct GridMap {
        /// The grid we're finding a path through
        /// TODO Make a reference, and debug the lifetime
        grid: Grid<bool>,
        // The path(s) we're building through the grid
        map: Grid<PathSquare>,
    }

    impl GridMap {
        /// Constructor
        pub fn new(grid: Grid<bool>) -> Self {
            let rows = grid.rows;
            let cols = grid.cols;
            Self {
                grid: grid,
                map: Grid::new(rows, cols),
            }
        }
        // Find the best path through a grid
        fn best_path(&self) -> Vec<Direction> {
            unimplemented!();
        }
    }

    // PathSquares save the calculated cost & direction at a specific square
    #[derive(Clone, Default)]
    struct PathSquare {
        cost: usize,
        direction: Direction,
    }

    // Direction of travel from a square
    #[derive(Clone)]
    pub enum Direction {
        Nowhere,
        Up,
        Down,
        Left,
        Right,
    }

    impl Default for Direction {
        fn default() -> Direction { Direction::Nowhere }
    }

    #[cfg(test)]
    mod tests {
        #[test]
        fn it_works() {
            assert_eq!(2 + 2, 4);
        }
    }
}

#[cfg(test)]
mod grid_tests {
    use super::Grid;

    #[test]
    fn new_grid() {
        // Create an immutable Grid of booleans, initialized to false.
        let g: Grid<bool> = Grid::new(5, 5);
        for vec in g.grid.iter() {
            for element in vec.iter() {
                assert_eq!(*element, false);
            }
        }
        // Using iterator adapaters:
        let b = g.grid
            // mut Iter<T>; Get an iterator over &bool
            .iter()
            // Flatten our nested vectors
            // mut FlatMap<
            //   Iter<bool>, -- The iterator of iterators
            //   IntoIterator, -- Nested iterators must support IntoIterator
            //   FnMut(Self::Item) -> U -- Function to apply to nested iterators
            // >
            .flat_map(|x| x)
            // Aggregate bools using OR
            .fold(false, |acc, x| acc || *x);
        assert_eq!(b, false);
    }
}
