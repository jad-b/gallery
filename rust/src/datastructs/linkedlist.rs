//! A doubly-linked list implementation.

struct DLinkedNode<T> {
    next: Option<Box<DLinkedNode<T>>>,
    prev: Option<Box<DLinkedNode<T>>>,
    data: T,
}

impl<T: PartialEq + PartialOrd> DLinkedNode<T> {

    /// Search for an element, comparing on element equality.
    fn search(&self, elem: T) -> Option<&Self> {
        if self.data == elem {
            return Some(&self)
        }
        match self.next {
            None =>  None,
            Some(n) => n.search(elem),
        }
    }

    /// Insert a new element before the Node.
    fn prepend(&mut self, elem: T) -> &Self {
        let newNode = DLinkedNode{
            next: Some(Box::new(*self)),
            prev: self.prev,
            data: elem,
        };
        // Prepare node
        let pkg_node = Some(Box::new(newNode));
        if let Some(n) = self.prev {
            let prev = &mut n; //  Mutable borrow
            prev.next = pkg_node;
        }
        // New predecessor is new node
        self.prev = pkg_node;
        &newNode
    }

    /// Append a new node after the Node.
    fn append(&mut self, elem: T) -> &Self {
        let newNode = DLinkedNode{
            next: self.next,
            prev: Some(Box::new(*self)),
            data: elem,
        };
        let pkg_node = Some(Box::new(newNode));
        if let Some(n) = self.next {
            let next = &mut n; // Mutable borrow
            next.prev = pkg_node;
        }
        self.next = pkg_node;
        &newNode
    }

    /// Delete an element by identity
    fn delete(&self, elem: T) -> Option<&Self> {
        let node = self.search(elem);
        match node {
            None => None,
            Some(ref n) => { // Update node neighbor connections
                if let Some(nxt) = n.next {
                   nxt.prev = n.prev;
                }
                if let Some(prv)= n.prev {
                    prv.next = n.next;
                }
                Some(n)
            },
        }
    }

    /// Return the smallest element.
    fn minimum(&self) -> T {
        let mut lo = self.data;
        let mut curr = Some(Box::new(self));
        while let Some(n) = curr {
            if n.data < lo {
                lo = n.data
            }
            curr = n.next;
        }
        lo
    }
    /// Return the largest element
    fn maximum(&self) -> Option<T>{}
    /// Return the element following the given element.
    fn successor(&self, elem: T) -> Option<Self>{}
    /// Return the element preceding the given element.
    fn predecessor(&self, elem: T) -> Option<Self>{}
}

/// A doubly-linked list, implemented as an extension of the DLinkedNode operations above.
struct DList<T> {
    head: Option<DLinkedNode<T>>,
    tail: Option<DLinkedNode<T>>,
}

/*
impl<T> DList<T> {
    /// Search for an element, comparing on element equality.
    fn search(&self, elem: T) -> Option<T>{}
    /// Insert a new element into the list, prepending before the head.
    fn Insert(&self, elem: T){}
    /// Delete an element by identity
    fn delete(&self, elem: T) -> Option<DLinkedNode<T>>{}
    /// Return the smallest element.
    fn minimum(&self) -> Option<T>{}
    /// Return the largest element
    fn maximum(&self) -> Option<T>{}
    /// Return the node following the given element.
    fn successor(&self, elem: T) -> Option<DLinkedNode<T>>{}
    /// Return the node preceding the given element.
    fn predecessor(&self, elem: T) -> Option<DLinkedNode<T>>{}
}
*/
