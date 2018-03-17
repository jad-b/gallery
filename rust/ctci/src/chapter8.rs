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
}

mod robot_grid {
    use chapter8::Grid;

    pub fn main<T>(grid: &Grid<T>) -> &Vec<Direction>
        where T: Default + Clone
    {
        unimplemented!();
    }

    impl<T: Default + Clone> Grid<T> {
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

    struct PathGrid<PathSquare: Clone + Default> {
        grid: Grid<PathSquare>,
    }

    impl<T> PathGrid<T> where T: Default + Clone {
        fn best_path(&self) -> Vec<usize> {
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
    use chapter8::Grid;

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
