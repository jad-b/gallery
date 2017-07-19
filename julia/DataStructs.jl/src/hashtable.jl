export ChainedHashTable

struct KeyValue{K,V}
    key::K
    value::V
    KeyValue{K,V}(key::K, value::V) where {K,V} = new(key, value)
    # Incomplete Initialization
    KeyValue{K,V}(key::K) where {K,V} = new(key)
end

# Only compare for equality KeyValues on their key
Base.:(==)(kv::KeyValue{K,V}, other::KeyValue{K,V}) where {K,V} = kv.key == other.key
Base.:<(kv::KeyValue{K,V}, other::KeyValue{K,V}) where {K,V} =  kv.key < other.key

mutable struct ChainedHashTable{K,V}
    # An array of buckets
    data::Array{List,1}
    # Our custom hash function
    hash
    # Number of stored elements
    count::Int
end

ChainedHashTable{K,V}(n::Int, fn) where {K,V} =
    ChainedHashTable{K,V}(Array{List,1}(n), fn, UInt(0))
# Create a hash table of size n
ChainedHashTable{K,V}(n::Int) where {K,V} = ChainedHashTable{K,V}(n, x -> (hash(x)%Int) & (n-1) + 1)

Base.length(h::ChainedHashTable{K,V}) where {K,V} = h.count

# Only compare for equality KeyValues on their key
function Base.:(==)(h::ChainedHashTable{K,V}, other::ChainedHashTable{K,V}) where {K,V}
    if length(h) != length(other)
        return false
    end
    # For each key-value in 'h', search the other hash table for the same
    # key-value pair
    for kv in h
        try
            value = other[kv.key]
            if value != kv.value
                return false
            end
        catch
            return  false
        end
    end
    true
end

# Test if the array slot has been defined
isdef(h::ChainedHashTable, i::Int) = isassigned(h.data, i)
isdef(arr::Array{List,1}, i::Int) = isassigned(arr, i)

function Base.:getindex(h::ChainedHashTable{K,V}, key::K) where {K,V}
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

function Base.:search(h::ChainedHashTable{K,V}, key::K) where {K,V}
    getindex(h,key)
end

# Insert a (key,value) entry into the Hash Table, returning the modified hash
# table.
function Base.:insert!(h::ChainedHashTable{K,V}, key::K, value::V) where {K,V}
    idx = h.hash(key)
    h.count += Int(insertkv(h.data, idx, KeyValue{K,V}(key,value)))
    h
end

# Returns true if an insert occurred, 0 if an update.
function insertkv(arr::Array{List,1}, idx::Int, kv::KeyValue{K,V}) where {K,V}
    if !isdef(arr, idx)
        arr[idx] = List()
        oldsz = 0
    else
        oldsz = length(arr[idx])
    end
    newsz = length(upsert!(arr[idx], kv))
    return newsz > oldsz
end

# Delete a key-value from the dictionary, returning the hash table.
# Alternatively, we could throw a KeyError on erroneous deletes, but
# that breaks the idempotency
function Base.:delete!(h::ChainedHashTable{K,V}, key::K) where {K,V}
    idx = h.hash(key)
    if isdef(h, idx)
        list::List = h.data[idx]
        delete!(list, KeyValue{K,V}(key))
        h.count -= 1
    end
    h
end

const CHTState = Tuple{Int,Nullable{ListNode}}

function Base.start(iter::ChainedHashTable{K,V}) where {K,V}
    # Not directly returned for the type assert
    println("hashtable size=$(iter.count)")
    rv::CHTState = next_node(iter, 1)
    return rv
end

function Base.done(iter::ChainedHashTable{K,V}, state::CHTState) where {K,V}
    state[1] > length(iter.data)
end

# Return the KeyValue of the current state, and prepare the next state.
function Base.next(iter::ChainedHashTable{K,V}, state::CHTState) where {K,V}
    println(state)
    # Invariant: The node within the state is always non-null
    node = state[2]
    @assert !isnull(node) "ListNode shouldn't be null; enforced by 'done()'"
    # Extract the value for the current state
    value::KeyValue{K,V} = get(node).elem
    if isnull(get(node).next) # End of list;
        #@assert iter.data[state[1]].tail == node
        # Find the next chain in the hash table
        return (value, next_node(iter, state[1]+1))
    end
    (value, (state[1], get(node).next))
end

# Return the next defined array slot and the head node of the list found there.
function next_node(h::ChainedHashTable{K,V}, index::Int) where {K,V}
    i = index
    while i <= length(h.data)
        if isdef(h, i)
            list::List = h.data[i]
            rv1::CHTState = (i, list.head)
            return rv1
        end
        i += 1
    end
    rv2::CHTState = (i, Nullable{ListNode}()) # Nothing found.
    rv2
end

function Base.:resize!(h::ChainedHashTable{K,V}, size::Int=length(h.data)<<1) where {K,V}
    arr = Array{List,1}(size)
    println("new array size is $size")
    count = 0
    for kv in h # Iterate through KeyValues, inserting into new array
        idx = h.hash(kv.key)
        insertkv(arr, idx, kv)
        count += 1
    end
    @assert count == h.count
    h.data = arr # replace
    h
end
