use std::ops::Index;

/// (row,col) position in a grid.
#[derive(Copy,Clone,Debug,PartialEq)]
struct Position(usize, usize);

impl Position {
    /// Given a starting position and a direction of travel, update our position.
    fn update(&self, d: Direction) -> Self {
        match d {
            Direction::Up => Position(self.0.saturating_sub(1), self.1),
            Direction::Down => Position(self.0 + 1, self.1),
            Direction::Left => Position(self.0, self.1.saturating_sub(1)),
            Direction::Right => Position(self.0, self.1 + 1),
            Direction::Nowhere => *self, // no-op, copy position
        }
    }
}

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

    fn contains(&self, pos: &Position) -> bool {
        (0..self.rows).contains(pos.0) && (0..self.cols).contains(pos.1)
    }

    fn safe_index(&self, pos: &Position) -> Option<&T> {
        match self.contains(pos) {
            true => Some(&self[*pos]),
            false => None
        }
    }
}

impl<T: Default + Clone> Index<Position> for Grid<T> {
    type Output = T;

    fn index(&self, pos: Position) -> &Self::Output {
        &self.grid[pos.0][pos.1]
    }
}

// Direction of travel from a square
#[derive(Clone,Copy,Debug,PartialEq,Eq,PartialOrd,Ord)]
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

/// Problem:
/// > Imagine a robot sitting on the upper left corner of a grid with `r` rows and `c` columns.
/// > The robot can only move in two directions, right and down, but certain cells are "off limits"
/// > such that the robot cannot step on them. Design an algorithm to find a path for the robot from
/// > the top left to the bottom right.
/// McDowell (2016, Problem 8.2)
pub mod robot_grid {
    use std::cmp::Ordering;
    // super refers to the parent module
    use super::{Direction,Grid,Position};

    pub fn solve(grid: &Grid<bool>) -> Vec<Direction> {
        GridMap::new(grid).best_path(Position(0,0))
    }

    /// A map of paths through a given grid.
    struct GridMap<'a> {
        /// The grid we're finding a path through
        grid: &'a Grid<bool>,
        /// The path(s) we're building through the grid
        map: Grid<PathSquare>,
        /// Whether this grid has been solved
        solved: bool,
    }

    impl<'a> GridMap<'a> {
        /// Constructor
        pub fn new(grid: &'a Grid<bool>) -> Self {
            let rows = grid.rows;
            let cols = grid.cols;
            Self {
                grid: grid,
                map: Grid::new(rows, cols),
                solved: false,
            }
        }

        /// Find the best path through a grid, given a current position.
        fn best_path(&mut self, start: Position) -> Vec<Direction> {
            // Could use cost of the starting square to determine Vec length.
            let mut path = Vec::new();
            let mut pos = start;
            while let Some(ps) = self.map.safe_index(&pos) {
                if let Direction::Nowhere = ps.direction {
                    break
                }
                path.push(ps.direction);
                pos = pos.update(ps.direction);
            }
            path
        }

        /// Navigate
        fn navigate(&mut self) -> &Self {
            for (r, row) in self.grid.grid.iter().enumerate().rev() {
                for (c, _) in row.iter().enumerate().rev() {
                    // Find the best move from this position on the grid
                    if let Some(ps) = self.best_move(&Position(r, c)) {
                        // We want to 'move' the value out of the return
                        self.map.grid[r][c] = ps;
                    }
                }
            }
            self.solved = true;
            self
        }

        /// Return the best move from a square, if there is one.
        fn best_move(&self, pos: &Position) -> Option<PathSquare> {
            let m_d = self.travel(pos, Direction::Down);
            let m_r = self.travel(pos, Direction::Right);
            match m_d {
                None => m_r,
                Some(d) => {
                    match m_r {
                        None => None,
                        Some(r) => Some(d.min(r))
                    }
                }
            }
        }

        /// Return the cost of moving a certain direction.
        fn travel(&self, pos: &Position, d: Direction) -> Option<PathSquare> {
            match self.map.safe_index(&pos.update(d)) {
                Some(ps) => Some(PathSquare {
                   cost: ps.cost + 1,
                   direction: d,
                }),
                None => None
            }
        }
    }

    // PathSquares save the calculated cost & direction at a specific square
    #[derive(Clone, Default, Eq, PartialEq)]
    struct PathSquare {
        cost: usize,
        direction: Direction,
    }

    impl PartialOrd for PathSquare {
        fn partial_cmp(&self, other: &PathSquare) -> Option<Ordering> {
            Some(self.cost.cmp(&other.cost))
        }
    }

    impl Ord for PathSquare {
        fn cmp(&self, other: &PathSquare) -> Ordering {
            self.cost.cmp(&other.cost)
        }
    }

    #[cfg(test)]
    mod tests {
        use super::{Grid,Position};
        use super::Direction::*;

        #[test]
        fn grid_3x3() {
            let g = Grid {
                rows: 3,
                cols: 3,
                grid: vec![
                    vec![true, true, true],
                    vec![true, false, true],
                    vec![true, false, true]
                ],
            };
            let exp = vec![Right, Right, Down, Down];
            let obs = super::solve(&g);
            assert_eq!(obs, exp)
        }

        #[test]
        fn update() {
            // Up
            assert_eq!(Position(0,0).update(Up), Position(0, 0));
            // Down
            assert_eq!(Position(0,0).update(Down), Position(1, 0));
            // Left
            assert_eq!(Position(0,0).update(Left), Position(0, 0));
            // Right
            assert_eq!(Position(0,0).update(Right), Position(0, 1));
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
