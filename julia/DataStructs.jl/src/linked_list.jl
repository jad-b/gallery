export List, ListNode,upsert!,minimum

mutable struct ListNode
    elem
    next::Nullable{ListNode}
    prev::Nullable{ListNode}
end
ListNode(x) = ListNode(x, Nullable{ListNode}(), Nullable{ListNode}())

mutable struct List
    head::Nullable{ListNode}
    tail::Nullable{ListNode}
    minimum::Nullable{Any}
    count::Int64
end
List() = List(Nullable{ListNode}(), Nullable{ListNode}(), Nullable{Any}(), 0)

Base.:length(list::List) = list.count

# Returns Nullable{T}, where `T` is the tye of `ListNode.elem`.
function Base.:search(list::List, x)
    node = search_node(list, x)
    if isnull(node)
        Nullable{Any}()
    else
        Nullable(get(node).elem)
    end
end

# Returns a Nullable{ListNode}
function search_node(list::List, x)
    curr = list.head
    while !isnull(curr) && get(curr).elem != x
        curr = get(curr).next
    end
    curr
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

function Base.:insert!(list::List, x)
    node = ListNode(x, list.head, Nullable{ListNode}())
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
    list
end

function upsert!(list::List, x)
    node = search_node(list, x)
    if !isnull(node)
        get(node).elem = x
        list
    else
        insert!(list, x)
    end
end

function Base.:delete!(list::List, x)
    node::Nullable{ListNode} = search_node(list, x)
    if isnull(node)
        node
    else
        n::ListNode = get(node)
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
# List -> Nullable{T}, where T: typeof(ListNode.elem)
function Base.:minimum(list::List)
   return list.minimum
end
