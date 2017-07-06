struct KeyValue{K,V}
    key::K
    value::V
    KeyValue{K,V}(key::K, value::V) where {K,V} = new(key, value)
    # Incomplete Initialization
    KeyValue{K,V}(key::K) where {K,V} = new(key)
end

# Only compare for equality KeyValues on their key
function Base.:(==)(kv::KeyValue{K,V}, other::KeyValue{K,V}) where {K,V}
    kv.key == other.key
end

function isless(kv::KeyValue{K,V}, other::KeyValue{K,V}) where {K,V}
    kv.key < other.key
end

mutable struct ChainedHashTable{K,V}
    # An array of buckets
    data::Array{List,1}
    # Our custom hash function
    hash
    # Number of stored elements
    count::UInt64
end

ChainedHashTable{K,V}(n::Int, fn) where {K,V} =
    ChainedHashTable{K,V}(Array{List,1}(n), fn, UInt64(0))
# Create a hash table of size n
ChainedHashTable{K,V}(n::Int) where {K,V} = ChainedHashTable{K,V}(n, x -> (hash(x)%Int) & (n-1) + 1)

function Base.length(h::ChainedHashTable{K,V}) where {K,V}
    h.count
end

# Only compare for equality KeyValues on their key
function Base.:(==)(h::ChainedHashTable{K,V}, other::ChainedHashTable{K,V}) where {K,V}
    if h.count != other.count
        return false
    end
    for i=1:h.length
        if !(isdef(h,i) && isdef(other,i)) || h.data[i] != other.data[i]
            return false
        end
    end
    true
end

# Test if the array slot has been defined
isdef(h::ChainedHashTable, i::Int) = isassigned(h.data, i)


function Base.:search(h::ChainedHashTable{K,V}, key::K) where {K,V}
    idx = h.hash(key)
    if isdef(h, idx)
        list::List = h.data[idx] # Get list at position
        kv::Nullable{KeyValue} = search(list, KeyValue{K,V}(key))
        if !isnull(kv)
            return get(kv).value
        end
    end
    throw(KeyError(key))
end

# Insert a (key,value) entry into the Hash Table.
function insert!(h::ChainedHashTable{K,V}, key::K, value::V) where {K,V}
    idx = h.hash(key)
    if !isdef(h, idx)
        h.data[idx] = List()
    end
    # TODO Add upsert method to LinkedList
    insert(h.data[idx], KeyValue{K,V}(key, value))
    h.count += 1
    h
end

function delete!(h::ChainedHashTable{K,V}, key::K) where {K,V}
    idx = h.hash(key)
    if isdef(h, idx)
        list::List = h.data[idx]
        delete(list, KeyValue{K,V}(key))
        h.count -= 1
        # Alternatively, we could throw a KeyError on erroneous deletes, but
        # that breaks the idempotency
    end
    h
end

const CHTState = Tuple{Int64,Nullable{Node}}
function start(iter::ChainedHashTable{K,V}) where {K,V}
    next_defined_node(iter, 1)
end
function done(iter::ChainedHashTable{K,V}, state::CHTState) where {K,V}
    state[1] > length(iter.data)
end
function next(iter::ChainedHashTable{K,V}, state::CHTState) where {K,V}
    # Invariant: The node within the state is always non-null
    node = get(state[2])
    value::KeyValue{K,V} = node.elem
    if isnull(node.next)
        (value, next_defined_node(iter, state[1]+1))
    end
    (value, CHTState(state[1], node.next))
end

# Return the next defined array slot and the head node of the list found there.
function next_defined_node(h::ChainedHashTable{K,V}, index::Int64) where {K,V}
    i = index
    while i < length(data)
        if isdef(h, i)
            list::List = h.data[i]
            return CHTState(i, list.head)
        end
        i += 1
    end
    CHTState(i, Nullable{Node}()) # Nothing found.
end
