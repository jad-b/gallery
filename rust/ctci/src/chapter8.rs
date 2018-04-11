/// Problem:
/// > Imagine a robot sitting on the upper left corner of a grid with `r` rows and `c` columns.
/// > The robot can only move in two directions, right and down, but certain cells are "off limits"
/// > such that the robot cannot step on them. Design an algorithm to find a path for the robot from
/// > the top left to the bottom right.
/// McDowell (2016, Problem 8.2)
pub mod robot_grid {
    use std::cmp::Ordering;
    // super refers to the parent module
    use grid::{Direction,Grid,Position};
    use std::fmt;

    pub fn solve(grid: &Grid<bool>) -> Vec<Direction> {
        let mut gm = GridMap::new(grid);
        gm.navigate();
        gm.best_path(Position(0,0))
    }

    /// A map of paths through a given grid.
    struct GridMap<'a> {
        /// The grid we're finding a path through
        grid: &'a Grid<bool>,
        /// The path(s) we're building through the grid
        map: Grid<PathSquare>,
    }

    impl<'a> GridMap<'a> {
        /// Constructor
        pub fn new(grid: &'a Grid<bool>) -> Self {
            let rows = grid.rows;
            let cols = grid.cols;
            Self {
                grid: grid,
                map: Grid::new(rows, cols),
            }
        }

        /// Build the best path through a navigated grid, given a starting position.
        fn best_path(&mut self, start: Position) -> Vec<Direction> {
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
        fn navigate(&mut self) -> &Self {
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
            println!("{}", self.map);
            self
        }

        /// Return the best move from a square, if there is one.
        fn best_move(&self, pos: &Position) -> Option<PathSquare> {
            // TODO Only return Some(ps) when a valid move is presented, requiring
            // 1. Be within the board
            // 2. Note be a obstructed (not false)
            // 3. Have a Direction other than Nowhere
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
        fn travel(&self, pos: &Position, d: Direction) -> Option<PathSquare> {
            let dest = pos.update(d);
            match self.grid.safe_index(&dest) { // Check if the dest is available.
                Some(&b) if b =>  Some(PathSquare{
                    cost: self.map[dest].cost + 1,
                    direction: d
                }),
                _ => None
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

    impl fmt::Display for PathSquare {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "({},{})", self.direction, self.cost)
        }
    }

    #[cfg(test)]
    mod tests {
        use grid::Grid;
        use grid::Direction::*;

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
    }
}
