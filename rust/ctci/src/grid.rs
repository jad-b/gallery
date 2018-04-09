use std::ops::Index;
use std::fmt;

pub struct Grid<T: Default + Clone> {
    pub rows: usize,
    pub cols: usize,
    pub grid: Vec<Vec<T>>,
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

    pub fn contains(&self, pos: &Position) -> bool {
        (0..self.rows).contains(pos.0) && (0..self.cols).contains(pos.1)
    }

    pub fn safe_index(&self, pos: &Position) -> Option<&T> {
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

impl<T: Default + Clone + fmt::Display> fmt::Display for Grid<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut msg = String::from("");
        for row in self.grid.iter() {
            msg.push_str("[ ");
            for val in row.iter() {
                msg.push_str(&format!(" {} ", val));
            }
            msg.push_str("]\n");
        }
        write!(f, "{}", msg)
    }
}

/// (row,col) position in a grid.
#[derive(Copy,Clone,Debug,PartialEq)]
pub struct Position(pub usize, pub usize);

impl Position {
    /// Given a starting position and a direction of travel, update our position.
    pub fn update(&self, d: Direction) -> Self {
        match d {
            Direction::Up => Position(self.0.saturating_sub(1), self.1),
            Direction::Down => Position(self.0 + 1, self.1),
            Direction::Left => Position(self.0, self.1.saturating_sub(1)),
            Direction::Right => Position(self.0, self.1 + 1),
            Direction::Nowhere => *self, // no-op, copy position
        }
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

impl fmt::Display for Direction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match self {
            &Direction::Nowhere => "Nowhere",
            &Direction::Up => "Up",
            &Direction::Down => "Down",
            &Direction::Left => "Left",
            &Direction::Right => "Right",
        };
        write!(f, "{}", name)
    }
}

#[cfg(test)]
mod grid_tests {
    use super::{Grid,Position};
    use super::Direction::*;

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
