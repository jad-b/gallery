#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}

pub trait Contiguous {
    /// Search for an element, comparing on element equality.
    fn search<T: PartialEq>(&self, elem: T) -> Option<T>;
    /// Insert a new element.
    fn insert<T>(&self, elem: T);
    /// Delete an element by identity
    fn delete<T: PartialEq>(&self, elem: T);
    /// Return the smallest element.
    fn minimum<T>(&self) -> Option<T>;
    /// Return the largest element
    fn maximum<T>(&self) -> Option<T>;
    /// Return the element following the given element.
    fn successor<T: PartialOrd>(&self, elem: T) -> Option<T>;
    /// Return the element preceding the given element.
    fn predecessor<T: PartialOrd>(&self, elem: T) -> Option<T>;
}

struct Node<T> {
    next: Option<Box<Node<T>>>,
    prev: Option<Box<Node<T>>>,
    elem: T,
}

struct List<T> {
    head: Option<Node<T>>,
    tail: Option<Node<T>>,
}
