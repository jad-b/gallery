module LinkedList where

{-
* `search(L, k)`: Return a pointer to the first element in `L` that contains `k`.
* `insert(L, x)`: Given an element `x`, add it to the head of the list.
* `delete(L, x)`: Given an element `x`, remove it from the list.
* `minimum(L)`: Find smallest-keyed element in L
* `maximum(L)`: Find the largest-keyed element in L.
* `successor(L,x)`: Assuming `L` is totally-ordered, return the next element
  after `x`.
* `predecessor(L,x)`: Assuming `L`  is totally ordered, return the next
  element smaller than `x`.
-}

data LinkedList a = LinkedList
    { head :: Maybe (Node a)
    , tail :: Maybe (Node a)
    } deriving Show

data Node a = Node
    { next :: Maybe (Node a)
    , prev :: Maybe (Node a)
    , elem :: a
    } deriving Show

{- Search a linked list. Returns a Maybe {element} that matches the given
 - element. Thus, the search depends on the Eq implementation for the elements.
 - Tricky part was getting the Maybe/Node types straight within the helper
 - function.
 -}
search :: (Eq a) => LinkedList a -> a -> Maybe a
search ll x = trawl (LinkedList.head ll)
    where trawl mn = case mn of
            Nothing -> Nothing
            Just node -> if (LinkedList.elem node) == x -- Compare on element equality
                         then Just (LinkedList.elem node) -- Return this element
                         else trawl (LinkedList.next node) -- Keep looking

insert :: LinkedList a -> a -> LinkedList a
insert (LinkedList { LinkedList.head = Nothing } ) x =
    let n = Just (Node {next = Nothing, prev = Nothing, LinkedList.elem = x})
    in LinkedList n n
insert ll@(LinkedList {LinkedList.head=lHead, LinkedList.tail=lTail}) x =
    let n = Just (Node lHead Nothing x) -- Create our new node, pointing to old head
    in do
        let y = lHead {prev=n} -- re-assign old head node's prev to new node
        ll {LinkedList.head=n}  -- Update LL with new head
