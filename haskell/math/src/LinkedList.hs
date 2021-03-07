module LinkedList where
-- An implementation of a doubly-linked list.

-- TODO Make List foldable
data LinkedList a = LinkedList
    { head :: Node a
    , tail :: Node a
    } deriving Show

data Node a = NullNode -- Empty beginning or end
    | Node
      { next :: Node a
      , prev :: Node a
      , elem :: a
      } deriving Show


-- `search(L, k)`: Return a pointer to the first element in `L` with key `k`.
search :: (Eq a) => LinkedList a -> a -> Node a
search ll@(LinkedList {LinkedList.head=lHead}) x = trawl lHead
    where trawl n = case n of
            NullNode -> NullNode
            Node next _ elem -> if elem == x -- Compare on element equality
                                 then n -- Return this element
                                 else trawl next -- Keep looking

-- `insert(L, x)`: Given an element `x`, add it to the head of the list.
insert :: LinkedList a -> a -> LinkedList a

-- Easy case: List is empty
insert ll@(LinkedList { LinkedList.head = NullNode} ) x =
    ll {LinkedList.head=(Node NullNode NullNode x)}

-- Harder case: List isn't empty. We need to create a new node that points to
-- the old head while setting the old head to point back to the new node. We
-- avoid having to sequentially update state by using the existing old head,
-- anchoring a new node off its `prev` attribute, and using that to extract the
-- new head node.
insert ll@(LinkedList {LinkedList.head=lHead}) x =
    -- Create a new head, retrieved from the updated *old* head
    ll {LinkedList.head=prev(prepend lHead)}
    where
        -- Update the given node with a new node prepended
        prepend n = lHead { prev = Node lHead NullNode x}

-- `delete(L, k)`: Given a key to an element `k`, remove it from the list.
delete :: Eq a => LinkedList a -> a -> Node a
delete ll k =
    case search ll k of
        NullNode -> NullNode
        dn@(Node next prev elem) -> do
            -- Guessing this will leave prev.prev.next pointing at the old form
            -- of prev, and vice-versa for next.next.prev. So I bet it doesn't
            -- work.
            let prev = prev { LinkedList.next = next }
            let next = next { LinkedList.prev = prev }
            dn

-- `minimum(L)`: Find smallest-keyed element in L
minimum :: Ord a => LinkedList a -> a
minimum (LinkedList {LinkedList.head=lHead}) = trawl (LinkedList.elem lHead) (LinkedList.next lHead)
    where trawl min node = case node of
            NullNode -> min
            Node next _ elem -> if elem < min
                                then trawl elem next
                                else trawl min next

-- `maximum(L)`: Find the largest-keyed element in L.
maximum :: Ord a => LinkedList a -> a
maximum (LinkedList {LinkedList.head=lHead}) = trawl (LinkedList.elem lHead) (LinkedList.next lHead)
    where trawl max node = case node of
            NullNode -> max
            Node next _ elem -> if elem < max
                                then trawl elem next
                                else trawl max next

-- `successor(L,x)`: Assuming `L` is totally-ordered, return the next element
--  after `x`.
successor :: Ord a => LinkedList a -> a -> Node a
successor ll k =
    case search ll k of
        NullNode -> NullNode
        Node next _ _ -> next

-- `predecessor(L,x)`: Assuming `L` is totally ordered, return the next
--  element smaller than `x`.
predecessor :: Ord a => LinkedList a -> a -> Node a
predecessor ll k =
    case search ll k of
        NullNode -> NullNode
        Node _ prev _ -> prev
