mutable struct Node
    elem
    next::Nullable{Node}
    prev::Nullable{Node}
end
Node(x) = Node(x, Nullable{Node}(), Nullable{Node}())

mutable struct List
    head::Nullable{Node}
    tail::Nullable{Node}
    minimum::Nullable{Any}
    count::Int64
end
List() = List(Nullable{Node}(), Nullable{Node}(), Nullable{Any}(), 0)

# Returns a Nullable{Node}
function search_node(list::List, x)
    curr = list.head
    while !isnull(curr) && get(curr).elem != x
        curr = get(curr).next
    end
    curr
end

# Returns Nullable{T}, where `T` is the tye of `Node.elem`.
function Base.:search(list::List, x)
    node = search_node(list, x)
    if isnull(node)
        Nullable{Any}()
    else
        Nullable(get(node).elem)
    end
end

# Find the smallest value within the linked list
function find_minimum(list::List)
    if isnull(list.head)
        Nullable{Any}()
    else
        head = get(list.head)
        min = head.elem
        curr = head.next
        while !isnull(curr)
            val = get(curr).elem
            if val < min
                min = val
            end
        end
        Nullable(min)
    end
end

function insert(list::List, x)
    node = Node(x, list.head, Nullable{Node}())
    if isnull(list.head) # Point head & tail at new node
        list.tail = Nullable(node)
    else # Update existing head to point back to new node
        get(list.head).prev = Nullable(node)
    end
    list.head = Nullable(node)
    # Update minimum tracker
    if isnull(list.minimum) || x < get(list.minimum)
        list.minimum = Nullable(x)
    end
    list.count += 1
end

function delete(list::List, x)
    node::Nullable{Node} = search_node(list, x)
    if isnull(node)
        node
    else
        n::Node = get(node)
        if !isnull(n.prev)
            get(n.prev).next = n.next
        end
        if !isnull(n.next)
            get(n.next).prev = n.prev
        end
        if get(list.head) == n
            list.head = n.next
        end
        if get(list.tail) == n
            list.tail = n.prev
        end
        # Search list for new minimum
        list.minimum = find_minimum(list)
        list.count -= 1
        Nullable(n)
    end
end

# Return the smallest element.
# List -> Nullable{T}, where T: typeof(Node.elem)
function minimum(list::List)
   return list.minimum
end
