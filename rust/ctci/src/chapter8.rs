//! Imagine a robot sitting on the upper left corner of a grid with `r` rows and `c` columns.
//! The robot can only move in two directions, right and down, but certain cells are "off limits"
//! such that the robot cannot step on them. Design an algorithm to find a path for the robot from
//! the top left to the bottom right.
//!
//! Source: McDowell (2016, Problem 8.2)

/// Navigate a robot through a boolean grid, while only being able to move right or down.
///
/// # Examples
///
/// ```
/// use ctci::grid::Grid;
/// use ctci::grid::Direction::*;
/// use ctci::chapter8::solve_robot_grid;
///
/// let g = Grid {
///     rows: 3,
///     cols: 3,
///     grid: vec![
///         vec![true, true, true],
///         vec![true, false, true],
///         vec![true, false, true]
///     ],
/// };
/// let exp = vec![Right, Right, Down, Down];
/// let obs = solve_robot_grid(&g);
/// assert_eq!(obs, exp)
/// ```
use grid::{Direction,Grid,Position};
pub fn solve_robot_grid(grid: &Grid<bool>) -> Vec<Direction> {
    let mut gm = robot_grid::GridMap::new(grid);
    gm.navigate();
    gm.best_path(Position(0,0))
}

mod robot_grid {
    use std::cmp::Ordering;
    // super refers to the parent module
    use grid::{Direction,Grid,Position};
    use std::fmt;


    /// A map of paths through a given grid.
    pub struct GridMap<'a> {
        /// The grid we're finding a path through
        grid: &'a Grid<bool>,
        /// The path(s) we're building through the grid
        map: Grid<PathSquare>,
    }

    /// Overlay potential path solutions on top of the given grid to navigate.
    impl<'a> GridMap<'a> {
        /// Constructor
        pub fn new(grid: &'a Grid<bool>) -> Self {
            let rows = grid.rows;
            let cols = grid.cols;
            let mut gm = Self {
                grid: grid,
                map: Grid::new(rows, cols),
            };
            // Target square has a cost of zero.
            gm.map.grid[rows-1][cols-1] = PathSquare {
                cost: 0,
                direction: Direction::Nowhere,
            };
            gm
        }

        /// Build the best path through a navigated grid, given a starting position.
        pub fn best_path(&mut self, start: Position) -> Vec<Direction> {
            // Could use cost of the starting square to determine Vec length.
            let mut path = Vec::new();
            let mut pos = start;
            while let Some(& ref ps) = self.map.safe_index(&pos) {
                if let Direction::Nowhere = ps.direction {
                    break
                }
                path.push(ps.direction);
                pos = pos.update(ps.direction);
            }
            path
        }

        /// Navigate
        pub fn navigate(&mut self) -> &Self {
            for (r, row) in self.grid.grid.iter().enumerate().rev() {
                for (c, _) in row.iter().enumerate().rev() {
                    if !self.grid[Position(r, c)] {
                        continue
                    }
                    // Find the best move from this position on the grid
                    if let Some(ps) = self.best_move(&Position(r, c)) {
                        // We want to 'move' the value out of the return
                        self.map.grid[r][c] = ps;
                    }
                }
            }
            println!("\n{}", self.map);
            self
        }

        /// Return the best move from a square, if there is one.
        fn best_move(&self, pos: &Position) -> Option<PathSquare> {
            let m_d = self.travel(pos, Direction::Down);
            let m_r = self.travel(pos, Direction::Right);
            match (m_d, m_r) {
                (None, None) => None,
                (None, Some(r)) => Some(r),
                (Some(d), None) => Some(d),
                (Some(d), Some(r)) => Some(d.min(r)),
            }
        }

        /// Return the cost of moving a certain direction.
        ///
        /// Only return Some(ps) when a valid move is presented, requiring:
        /// 1. Be within the board
        /// 2. Not be obstructed (not false)
        fn travel(&self, pos: &Position, d: Direction) -> Option<PathSquare> {
            let dest = pos.update(d);
            self.grid.safe_index(&dest).and_then( // Be within the board
                |b| match b { // Not be obstructed
                    true => Some(&dest),
                    false => None,
                }
            ).and_then(
                |p| Some(PathSquare {
                    cost: self.map[*p].cost.saturating_add(1),
                    direction: d,
                })
           )
        }
    }

    // PathSquares save the calculated cost & direction at a specific square
    #[derive(Clone, Eq, PartialEq)]
    struct PathSquare {
        cost: usize,
        direction: Direction,
    }

    impl Default for PathSquare {
        fn default() -> PathSquare {
            PathSquare {
                cost: usize::max_value(),
                direction: Direction::Nowhere,
            }
        }
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

    impl fmt::Display for PathSquare {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "({},{})", self.direction, self.cost)
        }
    }

}
