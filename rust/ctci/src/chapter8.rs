mod robot_grid {
    use std::vec::Vec;

    pub fn main<T>(grid: &Vec<Vec<T>>) -> &Vec<PathSquare> {
        unimplemented!();
    }

    struct GridMap<T> {
        rows: T,
        cols: T,
        // 2D grid
        // grid: Vec<<Vec<bool>>,
    }

    impl<T> GridMap<T> {
        // Constructor?
        fn right(&self, row: T, col: T) -> Option<T> {
            unimplemented!();
        }
        fn down(&self, row: T, col: T) -> Option<T> {
            unimplemented!();
        }
        // Return the best move from a square, if there is one.
        fn best_move(&self, row: T, col: T) -> Option<&PathSquare> {
            unimplemented!();
        }
    }

    // TODO Extract Grid into common struct
    struct PathGrid<T> {
        rows: T,
        cols: T,
        // grid: Vec<<Vec<PathSquare>>,
    }

    impl<T> PathGrid<T> {
        // TODO Generify the i32
        fn best_path(&self) -> Vec<i32> {
            unimplemented!();
        }
    }

    // PathSquares save the calculated cost & direction at a specific square
    pub struct PathSquare {
        cost: i32,
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

