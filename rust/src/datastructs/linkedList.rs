//! Linked lists in Rust are a bit tricky, as they are _self-referntial_, or _recursive_ data
//! structures. Coupled with Rust's emphasis on safety and performance makes for some interesting
//! hoop-jumping.
//!
//! Let's assume a linked list is made up of Nodes, and a List points to at least one of these
//! nodes for keeping track of the "first" Node. In the case of a singly-linked list, a Node points
//! to its predecessor (or None). But ah hah - that gives Rust a case of the "infinite definition"
//! when it goes to allocate space on the stack for a Node. So we must wrap the Node in a Box,
//! or what you might call an "owning pointer", which points to typed space on the heap that it has
//! ownership of, but itself has a known size. That gives us a singly-linked list.
//!
//! But god help you if you want to have a notion of sub-lists within a list, or otherwise have
//! multiple things point to one Node. We need a shared version of `Box`, which Rust provides with
//! `Rc`. But `Rc` only provides **immutable** shared ownership, which ends up meaning we can add
//! new nodes, but can't get rid of 'em. To do _that_, we need to introduce _interior mutability_,
//! or the idea that we can enforce runtime protection around mutability with the help of
//! `RefCell`s. This is in contrast to Rust's normal m.o. of _exterior mutability_, where
//! mutability is tracked as part of the variable's type (`& mut` and `let mut x`). At least, I
//! think it's tracked in its type. Bah, I don't know. But, if a `RefCell` provides runtime
//! mutability protection, and `Rc` allows for reference-counted shared ownership, then wrapping a
//! `RefCell` in a `Rc` creates a variabe that can be both shared and mutated by its stockholders,
//! in a "safe", albeit hideously verbose, manner. At this point, you have added garbage-collection
//! + smart pointers to Rust without the benefit of years of GC optimizations.
//!
//! But it's safe! And that's the whole thing about Rust, right? Safety! The alternative dipping
//! toes into "unsafe" Rust, which I must admit, I am not cavalier about. If I wanted unsafe, I'd
//! write C. Well, no I wouldn't. I'd pony up and learn to write unsafe Rust. But I'll at least
//! wait until I have a good reason to.
//!
//! See [Learning Rust with entirely too many linked
//! lists](http://cglab.ca/~abeinges/blah/too-many-lists/book/) for an in-depth explanation of
//! these findings.

//! A doubly-linked list implementation.
use std::rc::{Rc,Weak};
use std::cell::RefCell;

/// A data structure with FIFO semantics.
trait Queue<T> {
    fn deqeueue(&mut self) -> Option<T>;
    fn enqueue(&mut self);
}

/// A data structure with LIFO semantics.
trait Stack<T> {
    /// Remove and return the most recently added value.
    fn pop(&mut self) -> Option<T>;
    /// Place a value on the stack.
    fn push(&mut self);
}

type Link<T> = Option<Rc<RefCell<DLinkedNode<T>>>>;

// Re-introduce Option. Maybe.
enum DLinkedNode<T> {
    Null,
    Node {
        // Strongly reference-counted 'next' pointers
        next: Link<T>,
        // Weakly ref-counted 'prev' pointers, wrapped in a RefCell to allow
        // mutability while still sharing the reference across many owners.
        prev: Link<T>,
        // Actual data
        data: T
    },
}

// http://cglab.ca/~abeinges/blah/too-many-lists/book/fourth-layout.html
impl<T: PartialEq + PartialOrd> DLinkedNode<T> {

    /// Search for an element, comparing on element equality.
    fn search(&self, elem: T) -> DLinkedNode<T> {
        match self {
            DLinkedNode::Null => DLinkedNode::Null,
            DLinkedNode::Node {ref next, ref data, ..} => {
                if *data == elem {
                    return self
                }
                next.search(elem)
            }
        }
    }

    /*
    /// Insert a new element before the Node.
    fn prepend(&mut self, elem: T) -> &DLinkedNode<T> {
        let newNode = DLinkedNode{
            next: Some(Box::new(self)),
            prev: self.prev,
            data: elem,
        };
        // Prepare node
        let pkg_node = Some(Box::new(newNode));
        if let Some(mut p) = self.prev {
            p.next = pkg_node;
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
    fn minimum(&self) -> &T {
        let mut lo = &self.data;
        let mut curr: Option<Box<DLinkedNode<T>>> = Some(Box::new(*self));
        while let Some(n) = curr {
            if n.data < *lo { // Compare values against each other
                lo = &n.data // Take a reference to the value
            }
            curr = n.next;
        }
        &lo
    }
    /// Return the largest element
    fn maximum(&self) -> &T {
        let mut hi = self.data;
        let curr = Some(Box::new(*self));
        while let Some(n) = curr {
            if n.data > hi {
                hi = n.data;
            }
        }
        &hi
    }
    /// Return the element following the given element.
    fn successor(&self, elem: T) -> Option<&Self>{
        match self.search(elem) {
            None => None,
            Some(n) => {
                match n.next {
                    None => None,
                    Some(nxt) => Some(&(*nxt)), // Deref Box, take ref
                }
            }
        }
    }

    /// Return the element preceding the given element.
    fn predecessor(&self, elem: T) -> Option<&Self> {
        match self.search(elem) {
            None => None,
            Some(n) => {
                match n.prev {
                    None => None,
                    Some(p) => Some(&(*p)),
                }
            }
        }
    }
    */
}

/// A doubly-linked list, implemented as an extension of the DLinkedNode operations above.
struct DList<T> {
    head: Link<T> ,
    tail: Link<T>,
}

impl Stack<T> for DList<T> {
    /// Remove and return the most recently added value.
    fn pop(&mut self) -> Option<T> {
        None
    }
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
